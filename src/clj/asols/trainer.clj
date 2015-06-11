(ns asols.trainer
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :refer [sum sum-of-squares]]
            [clojure.set :as set]
            [asols.data]
            [asols.commands]
            [asols.network :as net])
  (:import (asols.commands TrainOpts)
           (asols.data Entry)))

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
  (m/emap #(if (pos? %) % 0.0) in-vec))

(defmethod derivative ::relu
  [_ out-vec]
  (m/emap #(if (pos? %) 1.0 0.0) out-vec))

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

(defn keep-with-prob
  [vector keep-prob]
  (if (= 1.0 (double keep-prob))
    vector
    (m/emap
     (fn [val]
       (if (< (rand) keep-prob)
         val
         0.0))
     vector)))

(defn forward-pass
  "Performs forward pass, returns vector of vectors of layers outputs"
  ([net in-vec]
    (forward-pass net in-vec 1 1))
  ([{:keys [layers]} in-vec in-prob hidden-prob]
    (let [layers-count (count layers)]
     (loop [out-vectors [(keep-with-prob in-vec in-prob)]
            layer-i 0]
       (if (>= layer-i layers-count)
         out-vectors
         (let [is-out-layer? (= layer-i (dec layers-count))
               {:keys [edges-matrix biases] :as layer} (nth layers layer-i)
               weighted-sums (-> (last out-vectors)
                                 (m/mmul edges-matrix)
                                 (m/add! biases))
               out-vec (forward layer weighted-sums)
               final-out-vec (if is-out-layer?
                               out-vec
                               (keep-with-prob out-vec hidden-prob))]
           (recur (conj out-vectors final-out-vec)
                  (+ 1 layer-i))))))))

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
    (loop [layer-i (- (count layers) 2)
           deltas-vecs [deltas-vec]]
      (if (< layer-i 0)
        (reverse deltas-vecs)
        (let [layer (nth layers layer-i)
              next-layer (nth layers (+ layer-i 1))
              layer-out (nth forward-vecs (+ layer-i 1))
              next-layer-deltas-vec (vec->matrix (last deltas-vecs))
              deltas-vec (m/mul!
                           (derivative layer layer-out)
                           (first (m/mmul next-layer-deltas-vec
                                          (m/transpose (:edges-matrix next-layer)))))]
          (recur (- layer-i 1)
                 (conj deltas-vecs deltas-vec)))))))

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
  [e-count t-opts layer deltas-vec prev-delta-b]
  (let [{:keys [learning-rate momentum l2-lambda]} t-opts]
    (-> (m/mul -1 learning-rate deltas-vec)
        (m/add! (m/mul momentum prev-delta-b))
        (m/sub! (m/mul (:biases layer)
                       l2-lambda learning-rate (/ 1 e-count))))))

(defn get-initial-delta-w
  "Returns collection of matrixes of initial delta weights of layers"
  [net]
  (for [layer (:layers net)
        :let [[in-count out-count] (net/get-layer-shape layer)]]
    (m/zero-matrix in-count out-count)))

(defn get-initial-delta-b
  [net]
  (for [layer (:layers net)
        :let [[_ out-count] (net/get-layer-shape layer)]]
    (m/zero-vector out-count)))

(defn backprop-step-delta
  "Performs forward & backward passes of single data entry.
  Returns vector of [delta-weights delta-biases cost-val] where:
    delta-weights - coll of matrixes of weights deltas for each layer
    delta-biases - coll of vectors of biases deltas for each layer"
  [net entry e-count t-opts prev-delta-w prev-delta-b]
  (let [{:keys [input-vec target-vec]} entry
        {:keys [in-node-prob hidden-node-prob]} t-opts
        forward-vecs (forward-pass net input-vec in-node-prob hidden-node-prob)
        deltas-vecs (backward-pass net target-vec forward-vecs)]
    [(map
       #(backprop-delta-layer-weights e-count t-opts %1 %2 %3 %4)
       (:layers net)
       deltas-vecs
       forward-vecs
       prev-delta-w)
     (map
       #(backprop-delta-layer-biases e-count t-opts %1 %2 %3)
       (:layers net)
       deltas-vecs
       prev-delta-b)]))

(defn backprop-step
  "Performs forward & backward pass, tunes network weights & biases.
  Returns new network and weights deltas."
  [net entry e-count t-opts prev-delta-w prev-delta-b]
  (let [results (backprop-step-delta net entry e-count t-opts prev-delta-w prev-delta-b)
        [delta-weights delta-biases] results
        new-layers (map
                     (fn [{:keys [edges-matrix edges-mask biases] :as layer} delta-w delta-b]
                       (let [delta-w (m/mul delta-w edges-mask)
                             new-edges-matrix (m/add delta-w edges-matrix)
                             new-biases (m/add delta-b biases)]
                         (assoc layer :edges-matrix new-edges-matrix
                                      :biases new-biases)))
                     (:layers net)
                     delta-weights
                     delta-biases)]
    [(assoc net :layers (vec new-layers))
     delta-weights
     delta-biases]))

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
           entry-i 0
           curr-delta-w (get-initial-delta-w curr-net)
           curr-delta-b (get-initial-delta-b curr-net)]
      (if (>= entry-i e-count)
        curr-net
        (let [entry (nth entries entry-i)
              step-result (backprop-step curr-net entry e-count t-opts curr-delta-w curr-delta-b)
              [new-net new-delta-w new-delta-b] step-result]
          (recur new-net (inc entry-i) new-delta-w new-delta-b))))))

(defn train
  "Trains given network on a dataset during multiple epochs.
  If net-fn was given, returns vector of trained net and (map net-fn trained-networks),
  otherwise returns trained net."
  ([start-net entries t-opts]
    (train start-net entries t-opts nil))
  ([start-net entries t-opts net-fn]
   {:pre [(not (empty? entries))
          (instance? TrainOpts t-opts)
          (or (nil? net-fn) (fn? net-fn))]}
   (loop [net start-net
          iter-i 0
          results (transient [])]
     (if (< iter-i (:iter-count t-opts))
       (let [new-net (train-epoch net entries t-opts)
             result (if (nil? net-fn) nil (net-fn new-net))]
         (recur new-net (inc iter-i) (conj! results result)))
       (if (nil? net-fn)
         net
         [net (persistent! results)])))))

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
  (/ (sum (m/array (map #(calc-ca-entry net %) entries)))
     (count entries)))

