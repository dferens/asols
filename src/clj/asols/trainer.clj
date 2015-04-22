(ns asols.trainer
  (:require [clojure.core.matrix :as matrix]
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


(defn hidden-types
  "Returns all available hidden layers types"
  []
  (let [forward-implementers (into #{} (keys (methods forward)))
        derivative-implementers (into #{} (keys (methods derivative)))]
    (set/intersection forward-implementers derivative-implementers)))

(defn out-types
  "Returns all available output layers types"
  []
  (let [forward-implementers (into #{} (keys (methods forward)))
        out-deltas-implementers (into #{} (keys (methods out-deltas)))]
    (set/intersection forward-implementers out-deltas-implementers)))

;; Sigmoid hidden layer & output layer with euclidean loss

(defmethod forward ::sigmoid
  [_ in-vec]
  (matrix/div 1
              (matrix/add 1 (matrix/emap #(Math/exp %) (matrix/sub in-vec)))))

(defmethod derivative ::sigmoid
  [_ out-x]
  (* out-x (- 1 out-x)))

(defmethod out-deltas ::sigmoid
  [_ out-vec target-vec]
  (matrix/mul out-vec
              (matrix/sub 1 out-vec)
              (matrix/sub out-vec target-vec)))

;; ReLU hidden layer

(defmethod forward ::relu
  [_ in-vector]
  (matrix/emap (partial max 0) in-vector))

(defmethod derivative ::relu
  [_ out-x]
  (if (pos? out-x) 1 0))

;; Softmax output layer with log-likehood loss

(defmethod forward ::softmax
  [_ in-vec]
  (let [safe-exp #(-> % (max -500) (min 500) (Math/exp))
        exp-sums (matrix/emap safe-exp in-vec)
        total (matrix/esum exp-sums)]
    (matrix/div exp-sums total)))

(defmethod out-deltas ::softmax
  [_ out-vector target-vector]
  (matrix/sub out-vector target-vector))


;; Backprop implementation

(defn- forward-layer
  "Performs forward pass for single layer, returns map of nodes outputs"
  [net values layer]
  (let [in-vec (for [node (:nodes layer)
                     :let [in-edges (network/in-edges net node)]]
                 (sum (for [[node-from _ :as edge] in-edges]
                        (* (values node-from)
                           (network/get-weight net edge)))))
        out-vec (forward layer (matrix/array in-vec))]
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
  (let [out-vec-seq (map #(forward-values %) (:nodes layer))
        out-vec (matrix/array out-vec-seq)
        deltas-vector (out-deltas layer out-vec target-vec)]
    (zipmap (:nodes layer) deltas-vector)))

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

(defn backward
  "Performs backward pass, returns map of nodes deltas"
  [net target-vec forward-values]
  (let [out-layer (last (:layers net))
        layers (reverse (network/hidden-layers net))
        deltas-map (backward-out-layer out-layer forward-values target-vec)
        layer-reducer #(backward-layer net forward-values %1 %2)]
    (reduce layer-reducer deltas-map layers)))

(defn- backprop-delta-weights
  "Performs forward & backward pass, returns map of delta weights of edges"
  [net entry e-count train-opts prev-delta-weights]
  (let [{:keys [input-vec target-vec]} entry
        {:keys [learning-rate momentum l2-lambda]} train-opts
        forward-values (forward-pass net input-vec)
        deltas-map (backward net target-vec forward-values)]
    (into {} (for [[[node-from node-to :as e] weight] (:edges net)]
               (let [delta-weight (+ (* -1 learning-rate
                                        (deltas-map node-to)
                                        (forward-values node-from))
                                     (* momentum (get prev-delta-weights e 0))
                                     (* -1 l2-lambda learning-rate weight (/ e-count)))]
                 [e delta-weight])))))

(defn backprop-step
  "Performs forward & backward pass and tunes network weights.
  Returns new network and weights deltas being made."
  [net entry entries-count train-opts prev-delta-weights]
  (let [delta-weights (backprop-delta-weights net entry entries-count train-opts prev-delta-weights)
        new-net (update-in net [:edges] #(merge-with + % delta-weights))]
    [new-net delta-weights]))

(defn- train-entries
  [start-net entries t-opts]
  (let [e-count (count entries)]
    (loop [net start-net
           entries-left entries
           delta-w nil]
     (if (empty? entries-left)
       net
       (let [entry (first entries-left)
             [new-net new-delta-w] (backprop-step net entry e-count t-opts delta-w)]
         (recur new-net (rest entries-left) new-delta-w))))))

(defn train
  "Trains given network on dataset with given opts, returns new net"
  [start-net dataset t-opts]
  {:pre [(instance? Dataset dataset)
         (instance? TrainOpts t-opts)]}
  (let [entries (:train dataset)
        iter-count (:iter-count t-opts)]
    (loop [net start-net i 0]
      (if (< i iter-count)
        (recur (train-entries net entries t-opts) (inc i))
        net))))

(defn activate
  "Returns output vector of after passing given input vector on net's inputs"
  [net input-vector]
  (let [nodes-values (forward-pass net input-vector)
        out-vector-seq (map nodes-values (:nodes (last (:layers net))))]
    (matrix/array out-vector-seq)))

(defn calc-error-on-entry
  "Calculates error on given dataset entry for given network"
  [net training-record]
  {:pre [(instance? Entry training-record)]}
  (let [{:keys [input-vec target-vec]} training-record
        predicted-output (activate net input-vec)
        errors (into [] (matrix/sub target-vec predicted-output))]
    (/ (sum-of-squares errors)
       2)))

(defn calc-ca
  "Calculates classification accuracy on given collection of dataset entries"
  [net entries]
  (let [correct-entries (for [{:keys [input-vec target-vec] :as entry} entries
                              :let [out-vec (activate net input-vec)
                                    max-out (matrix/emax out-vec)
                                    predicted-class-i (.indexOf (into [] out-vec) max-out)]
                              :when (= 1.0 (matrix/mget target-vec predicted-class-i))]
                          entry)]
    (/ (count correct-entries)
       (count entries))))

(defn calc-error
  "Calculates error on given dataset entries for given network"
  [net entries]
  (sum (map #(calc-error-on-entry net %) entries)))
