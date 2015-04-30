(ns asols.trainer
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :refer [sum sum-of-squares]]
            [clojure.set :as set]
            [asols.network :as network]
            [asols.data]
            [asols.commands])
  (:import (asols.commands TrainOpts)
           (asols.data Entry Dataset)))

(defmulti forward
  "Accepts weighted sums of each neuron as input vector, computes output vector.
  Every network layer should implement this method."
  (fn [layer in-vector] (:type layer)))

(defmulti derivative
  "Computes activation function derivative.
  Only hidden layers should implement this method."
  (fn [layer out-x] (:type layer)))

(defmulti out-deltas
  "Computes deltas (errors) of output layer."
  (fn [layer out-vector target-vector] (:type layer)))

(defmulti cost
  "Computes cost"
  (fn [layer out-vector target-vector] (:type layer)))


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

;; Sigmoid hidden layer & output layer with euclidean loss

(defmethod forward ::sigmoid
  [_ in-vec]
  (m/div 1
         (m/add 1 (m/emap #(Math/exp %) (m/sub in-vec)))))

(defmethod derivative ::sigmoid
  [_ out-x]
  (* out-x (- 1 out-x)))

(defmethod out-deltas ::sigmoid
  [_ out-vec target-vec]
  (m/mul out-vec
         (m/sub 1 out-vec)
         (m/sub out-vec target-vec)))

(defmethod cost ::sigmoid
  [_ out-vec target-vec]
  (/ (m/esum (m/square (m/sub target-vec out-vec)))
     2))

;; ReLU hidden layer

(defmethod forward ::relu
  [_ in-vector]
  (m/emap (partial max 0) in-vector))

(defmethod derivative ::relu
  [_ out-x]
  (if (pos? out-x) 1 0))

;; Softmax output layer with log-likehood loss

(defmethod forward ::softmax
  [_ in-vec]
  (let [safe-exp #(-> % (max -500) (min 500) (Math/exp))
        exp-sums (m/emap safe-exp in-vec)
        total (m/esum exp-sums)]
    (m/div exp-sums total)))

(defmethod out-deltas ::softmax
  [_ out-vector target-vector]
  (m/sub out-vector target-vector))

(defmethod cost ::softmax
  [_ out-vec target-vec]
  (m/sub
    (m/esum
      (m/mul
        (m/emap #(Math/log %) (m/add out-vec 1E-15))
        target-vec))))


;; Backprop implementation

(defn- forward-layer
  "Performs forward pass for single layer, returns map of nodes outputs"
  [net values layer]
  (let [in-vec (for [node (:nodes layer)
                     :let [in-edges (network/in-edges net node)]]
                 (reduce +
                   (network/get-bias net node)
                   (for [[node-from _ :as edge] in-edges]
                     (* (values node-from)
                        (network/get-weight net edge)))))
        out-vec (forward layer (m/array in-vec))]
    (into values (map vector (:nodes layer) out-vec))))

(defn forward-pass
  "Performs forward pass, returns map of nodes outputs"
  [net input-vec]
  (let [input-layer (first (:layers net))
        nodes-values (zipmap (:nodes input-layer) input-vec)
        layers (rest (:layers net))
        layer-reducer #(forward-layer net %1 %2)]
    (reduce layer-reducer nodes-values layers)))

(defn- backward-out-layer
  "Performs backward pass for output layer, returns map of nodes deltas"
  [layer forward-values target-vec]
  (let [out-vec (m/array (map forward-values (:nodes layer)))
        cost-val (cost layer out-vec target-vec)
        deltas-vector (out-deltas layer out-vec target-vec)]
    [(zipmap (:nodes layer) deltas-vector)
     cost-val]))

(defn- backward-layer
  "Performs backward pass for hidden layer, returns map of nodes deltas"
  [net forward-values deltas-map layer]
  (into deltas-map (for [node (:nodes layer)]
                     (let [out-x (forward-values node)
                           out-edges (network/out-edges net node)
                           delta (* (derivative layer out-x)
                                    (sum (for [[_ node-to :as e] out-edges]
                                           (* (deltas-map node-to)
                                              (network/get-weight net e)))))]
                       [node delta]))))

(defn backward-pass
  "Performs backward pass, returns map of nodes deltas"
  [net target-vec forward-values]
  (let [out-layer (last (:layers net))
        layers (reverse (network/hidden-layers net))
        [deltas-map cost-val] (backward-out-layer out-layer forward-values target-vec)
        layer-reducer #(backward-layer net forward-values %1 %2)]
    [(reduce layer-reducer deltas-map layers)
     cost-val]))

(defn- backprop-delta-weights
  "Performs forward & backward pass, returns map of delta weights of edges and cost"
  [net entry e-count train-opts prev-delta-w]
  (let [{:keys [input-vec target-vec]} entry
        {:keys [learning-rate momentum l2-lambda]} train-opts
        forward-values (forward-pass net input-vec)
        [deltas-map cost-val] (backward-pass net target-vec forward-values)
        delta-w (into {} (for [[[node-from node-to :as e] weight] (:edges net)]
                           (let [delta-w (+ (* -1 learning-rate
                                               (deltas-map node-to)
                                               (forward-values node-from))
                                            (* momentum (get prev-delta-w e 0))
                                            (* -1 l2-lambda learning-rate weight
                                               (/ e-count)))]
                             [e delta-w])))
        delta-b (into {} (for [layer (rest (:layers net))
                               node (:nodes layer)
                               :let [delta-b (* -1 learning-rate
                                                (deltas-map node))]]
                           [node delta-b]))]
    [delta-w delta-b cost-val]))

(defn- backprop-step
  "Performs forward & backward pass and tunes network weights.
  Returns new network and weights deltas being made."
  [net entry entries-count t-opts prev-delta-w]
  (let [[delta-w delta-b cost-val] (backprop-delta-weights net entry entries-count t-opts prev-delta-w)
        new-net (-> net (update-in [:edges] #(merge-with + % delta-w))
                        (update-in [:biases] #(merge-with + % delta-b)))]
    [new-net delta-w cost-val]))

(defn train-epoch
  "Trains given network on a dataset for single epoch.
  Returns vector:
    [net           - trained network
     cost-val]     - average cost value"
  [start-net entries t-opts]
  (let [e-count (count entries)]
    (loop [curr-net start-net
           entries-left entries
           curr-delta-w nil
           cost-accum 0]
      (if (empty? entries-left)
        [curr-net (/ cost-accum e-count)]
        (let [entry (first entries-left)
              step-result (backprop-step curr-net entry e-count t-opts curr-delta-w)
              [new-net new-delta-w cost-val] step-result]
          (recur new-net
                 (rest entries-left)
                 new-delta-w
                 (+ cost-accum cost-val)))))))

(defn train
  "Trains given network on a dataset during multiple epochs.
  Returns trained net and average cost on train entries for last network."
  [start-net dataset t-opts]
  {:pre [(instance? Dataset dataset)
         (instance? TrainOpts t-opts)]}
  (let [entries (:train dataset)
        iter-count (:iter-count t-opts)]
    (loop [net start-net
           iter-i 0
           curr-cost nil]
      (if (< iter-i iter-count)
        (let [train-result (train-epoch net entries t-opts)
              [new-net new-cost] train-result]
          (recur new-net
                 (inc iter-i)
                 new-cost))
        [net curr-cost]))))

(defn activate
  "Returns output vector of after passing given input vector on net's inputs"
  [net input-vector]
  (let [nodes-values (forward-pass net input-vector)
        out-vector-seq (map nodes-values (:nodes (last (:layers net))))]
    (m/array out-vector-seq)))

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
        correct? (= 1.0 (m/mget target-vec predicted-class))]
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

