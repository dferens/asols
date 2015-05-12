(ns asols.trainer
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :refer [sum sum-of-squares]]
            [clojure.set :as set]
            [asols.data]
            [asols.commands]
            [asols.network :as net])
  (:import (asols.commands TrainOpts)
           (asols.data Entry Dataset)))

(m/set-current-implementation :vectorz)

(defmulti forward
  "Accepts weighted sums of each neuron as input vector, computes output vector.
  Every network layer should implement this method."
  (fn [layer in-vec] (:type layer)))

(defmulti derivative
  "Computes activation function derivative.
  Only hidden layers should implement this method."
  (fn [layer out-vec] (:type layer)))

(defmulti out-deltas
  "Computes deltas (errors) of output layer."
  (fn [layer out-vec target-vec] (:type layer)))

(defmulti cost
  "Computes cost"
  (fn [layer out-vec target-vec] (:type layer)))

(defn hidden-types
  "Returns all available hidden layers types"
  []
  (let [forward-impl (into #{} (keys (methods forward)))
        derivative-impl (into #{} (keys (methods derivative)))]
    (set/intersection forward-impl derivative-impl)))

(defn out-types
  "Returns all available output layers types"
  []
  (let [forward-impl (into #{} (keys (methods forward)))
        out-deltas-impl (into #{} (keys (methods out-deltas)))
        cost-impl (into #{} (keys (methods cost)))]
    (set/intersection forward-impl out-deltas-impl cost-impl)))

;; Linear layer

(defmethod forward ::linear [_ in-vec] in-vec)

(defmethod forward ::sigmoid
  [_ in-vec]
  (m/div 1
         (m/add 1 (m/emap #(Math/exp %) (m/sub in-vec)))))

(defmethod derivative ::sigmoid
  [_ out-vec]
  (m/mul out-vec (m/sub 1 out-vec)))

(defmethod out-deltas ::sigmoid
  [_ out-vec target-vec]
  (m/mul out-vec
         (m/sub 1 out-vec)
         (m/sub out-vec target-vec)))

(defmethod cost ::sigmoid
  [_ out-vec target-vec]
  (/ (m/esum (m/square (m/sub target-vec out-vec)))
     2))

(defmethod forward ::relu
  [_ in-vec]
  (m/emap (partial max 0) in-vec))

(defmethod derivative ::relu
  [_ out-vec]
  (m/emap #(if (pos? %) 1 0) out-vec))

;; Softmax output layer with log-likehood loss

(defmethod forward ::softmax
  [_ in-vec]
  (let [safe-exp #(-> % (max -500) (min 500) (Math/exp))
        exp-sums (m/emap safe-exp in-vec)
        total (m/esum exp-sums)]
    (m/div exp-sums total)))

(defmethod out-deltas ::softmax
  [_ out-vec target-vec]
  (m/sub out-vec target-vec))

(defmethod cost ::softmax
  [_ out-vec target-vec]
  (m/sub
    (m/esum
      (m/mul
        (m/emap #(Math/log %) (m/add out-vec 1E-15))
        target-vec))))

;; Backprop implementation

(defn forward-pass
  "Performs forward pass, returns vector of vectors of layers outputs"
  [{:keys [layers]} in-vec]
  (loop [out-vectors [in-vec]
         layers-left layers]
    (if (empty? layers-left)
      out-vectors
      (let [{:keys [edges-matrix] :as layer} (first layers-left)
            weighted-sums (m/mmul (last out-vectors) edges-matrix)
            out-vec (forward layer weighted-sums)]
        (recur (conj out-vectors out-vec)
               (rest layers-left))))))

(defn activate
  "Returns network output vector on given input vector."
  [net input-vec]
  (last (forward-pass net input-vec)))

(defn- vec->matrix
  [vector]
  (let [shape (m/shape vector)
        new-shape (cons 1 shape)]
    (m/reshape vector new-shape)))

(defn backward-pass
  "Performs backward pass, returns vector of vectors of layers deltas"
  [{:keys [layers]} target-vec forward-vecs]
  (let [out-vec (last forward-vecs)
        deltas-vec (out-deltas (last layers) out-vec target-vec)]
    (loop [layers-left (reverse (butlast layers))
           next-layers-left (reverse layers)
           deltas-vecs [deltas-vec]
           forward-vecs (drop 1 (reverse forward-vecs))]
      (if (empty? layers-left)
        (reverse deltas-vecs)
        (let [layer (first layers-left)
              next-layer (first next-layers-left)
              layer-out (first forward-vecs)
              next-layer-deltas-vec (vec->matrix (last deltas-vecs))
              deltas-vec (m/mul!
                           (derivative layer layer-out)
                           (first (m/mmul next-layer-deltas-vec
                                          (m/transpose (:edges-matrix next-layer)))))]
          (recur (rest layers-left)
                 (rest next-layers-left)
                 (conj deltas-vecs deltas-vec)
                 (rest forward-vecs)))))))

(defn backprop-delta-layer-weights
  "Calculates weights deltas of given layer, returns matrix of same
  shape as layer's edges matrix.

  Params:
   deltas-vec - vector of deltas (errors) of current layer
   prev-forward-vec - vector of outputs of previous layer"
  [e-count t-opts layer deltas-vec prev-forward-vec prev-delta-w-matrix]
  (let [{:keys [learning-rate momentum l2-lambda]} t-opts]
    (-> (m/mmul (m/transpose (vec->matrix prev-forward-vec))
                (vec->matrix deltas-vec))
        (m/mul! -1 learning-rate)
        (m/add! (m/mul momentum prev-delta-w-matrix))
        (m/sub! (m/mul (:edges-matrix layer)
                       l2-lambda learning-rate (/ 1 e-count))))))

(defn backprop-delta-layer-biases
  "Calculates biases deltas of layer, returns vector of same
  length as layer's biases vector.

  Params:
   - deltas-vec - vector of deltas (errors) of current layer"
  [t-opts deltas-vec]
  (m/mul -1
         (:learning-rate t-opts)
         deltas-vec))

(defn get-initial-delta-w
  "Returns collection of matrixes of initial delta weights of layers"
  [net]
  (for [layer (:layers net)
        :let [[in-count out-count] (net/get-layer-shape layer)]]
    (m/zero-matrix in-count out-count)))

(defn backprop-step-delta
  "Performs forward & backward passes of single data entry.
  Returns vector of [delta-weights delta-biases cost-val] where:
    delta-weights - coll of matrixes of weights deltas for each layer
    delta-biases - coll of vectors of biases deltas for each layer"
  [net entry e-count t-opts prev-delta-w]
  (let [{:keys [input-vec target-vec]} entry
        forward-vecs (forward-pass net input-vec)
        deltas-vecs (backward-pass net target-vec forward-vecs)]
    [(map
       #(backprop-delta-layer-weights e-count t-opts %1 %2 %3 %4)
       (:layers net)
       deltas-vecs
       forward-vecs
       prev-delta-w)
     (map
       #(backprop-delta-layer-biases t-opts %1)
       deltas-vecs)]))

(defn backprop-step
  "Performs forward & backward pass, tunes network weights & biases.
  Returns new network and weights deltas."
  [net entry e-count t-opts prev-delta-w]
  (let [results (backprop-step-delta net entry e-count t-opts prev-delta-w)
        [delta-weights delta-biases] results
        new-layers (map
                     (fn [{:keys [edges-matrix edges-mask biases] :as layer} delta-w delta-b]
                       (let [delta-w (m/mul delta-w edges-mask)
                             new-edges-matrix (m/add delta-w edges-matrix)
                             new-biases (m/add! delta-b biases)]
                         (assoc layer :edges-matrix new-edges-matrix
                                      :biases new-biases)))
                     (:layers net)
                     delta-weights
                     delta-biases)]
    [(assoc net :layers (vec new-layers))
     delta-weights]))

(defn calc-cost
  "Performs forward pass, calculates cost of out layer for each entry.
  Returns average cost value as double"
  [net entries]
  (let [out-layer (last (:layers net))
        cost-values (for [{:keys [input-vec target-vec]} entries
                          :let [out-vec (activate net input-vec)]]
                      (cost out-layer out-vec target-vec))]
    (m/div (sum (m/array cost-values))
           (count entries))))

(defn train-epoch
  "Trains given network on a dataset for single epoch, returns
  new net."
  [start-net entries t-opts]
  (let [e-count (count entries)]
    (loop [curr-net start-net
           entries-left entries
           curr-delta-w (get-initial-delta-w curr-net)]
      (if (empty? entries-left)
        curr-net
        (let [entry (first entries-left)
              step-result (backprop-step curr-net entry e-count t-opts curr-delta-w)
              [new-net new-delta-w] step-result]
          (recur new-net
                 (rest entries-left)
                 new-delta-w))))))

(defn train
  "Trains given network on a dataset during multiple epochs,
  returns new net."
  [start-net dataset t-opts]
  {:pre [(instance? Dataset dataset)
         (instance? TrainOpts t-opts)]}
  (let [entries (:train dataset)
        iter-count (:iter-count t-opts)]
    (loop [net start-net
           iter-i 0]
      (if (< iter-i iter-count)
        (let [new-net (train-epoch net entries t-opts)]
          (recur new-net (inc iter-i)))
        net))))

(defn- calc-distance-error-entry
  "Calculates error on single dataset entry for given network"
  [net entry]
  {:pre [(instance? Entry entry)]}
  (let [{:keys [input-vec target-vec]} entry
        predicted-output (activate net input-vec)
        errors (m/sub target-vec predicted-output)]
    (m/div (sum-of-squares errors)
           2)))

(defn- find-max
  "Returns index of max array element"
  [arr]
  (let [len (m/ecount arr)]
    (loop [curr-max-i 0
           elem-i 0]
      (if (< elem-i len)
        (recur (if (> (m/mget arr curr-max-i)
                      (m/mget arr elem-i))
                 curr-max-i
                 elem-i)
               (inc elem-i))
        curr-max-i))))

(defn- calc-ca-entry
  [net {:keys [input-vec target-vec]}]
  (let [out-vec (activate net input-vec)
        predicted-class (find-max out-vec)
        correct? (= 1.0 (double (m/mget target-vec predicted-class)))]
    (if correct? 1.0 0.0)))

(defn calc-squares-error
  "Calculates sum of squares error on given dataset entries"
  [net entries]
  (m/mget
    (sum
      (for [entry entries]
        (calc-distance-error-entry net entry)))))

(defn calc-ca
  "Calculates classification accuracy on given collection of dataset entries"
  [net entries]
  {:pre [(not (empty? entries))]}
  (/ (m/esum (m/array (for [e entries]
                        (calc-ca-entry net e))))
     (count entries)))