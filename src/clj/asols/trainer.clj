(ns asols.trainer
  (:require [clojure.core.matrix.stats :refer [sum sum-of-squares mean variance]]
            [clojure.set :as set]
            [asols.worker]
            [asols.network :as network])
  (:import (asols.worker TrainOpts)))

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


(defn hidden-layers-types
  "Returns all available hidden layers types"
  []
  (let [forward-implementers (into #{} (keys (methods forward)))
        derivative-implementers (into #{} (keys (methods derivative)))]
    (set/intersection forward-implementers derivative-implementers)))

(defn out-layers-types
  "Returns all available output layers types"
  []
  (let [forward-implementers (into #{} (keys (methods forward)))
        out-deltas-implementers (into #{} (keys (methods out-deltas)))]
    (set/intersection forward-implementers out-deltas-implementers)))

;; Sigmoid hidden layer & output layer with euclidean loss

(defn ^double sigmoid [^double x]
  (/ 1 (+ 1 (Math/exp (- x)))))

(defmethod forward ::sigmoid
  [layer in-vector]
  (map sigmoid in-vector))

(defmethod derivative ::sigmoid
  [layer out-x]
  (* out-x (- 1 out-x)))

(defmethod out-deltas ::sigmoid
  [layer out-vector target-vector]
  (map
    (fn [out-x target-x]
      (* (derivative layer out-x)
         (- out-x target-x)))
    out-vector target-vector))

;; ReLU hidden layer

(defmethod forward ::relu
  [layer in-vector]
  (map (partial max 0) in-vector))

(defmethod derivative ::relu
  [layer out-x]
  (if (pos? out-x) 1 0))

;; Softmax output layer with log-likehood loss

(defmethod forward ::softmax
  [layer in-vector]
  (let [safe-exp #(-> % (max -500) (min 500) (Math/exp))
        exp-sums (map safe-exp in-vector)
        total (sum exp-sums)]
    (for [x exp-sums]
      (/ x total))))

(defmethod out-deltas ::softmax
  [layer out-vector target-vector]
  (map - out-vector target-vector))


;; Backprop implementation

(defn- forward-layer
  "Performs forward pass for single layer, returns map of nodes outputs"
  [net values layer]
  (let [in-vector (for [node (:nodes layer)
                        :let [in-edges (network/in-edges net node)]]
                    (sum (for [[node-from _ :as edge] in-edges]
                           (* (values node-from)
                              (network/get-weight net edge)))))
        out-vector (forward layer (vec in-vector))]
    (into values (map vector (:nodes layer) out-vector))))

(defn forward-pass
  "Performs forward pass, returns map of nodes outputs"
  [net input-vector]
  (let [input-layer (first (:layers net))
        nodes-values (zipmap (:nodes input-layer) input-vector)
        layers (rest (:layers net))
        layer-reducer #(forward-layer net %1 %2)]
    (reduce layer-reducer nodes-values layers)))

(defn- backward-out-layer
  "Performs backward pass for output layer, returns map of nodes deltas"
  [layer forward-values target-vector]
  (let [out-vector (mapv #(forward-values %) (:nodes layer))
        deltas-vector (out-deltas layer out-vector target-vector)]
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
  [net target-vector forward-values]
  (let [out-layer (last (:layers net))
        layers (reverse (network/hidden-layers net))
        deltas-map (backward-out-layer out-layer forward-values target-vector)
        layer-reducer #(backward-layer net forward-values %1 %2)]
    (reduce layer-reducer deltas-map layers)))

(defn- backprop-delta-weights
  "Performs forward & backward pass, returns map of delta weights of edges"
  [net [input-vector output-vector] train-opts prev-delta-weights]
  (let [{:keys [learning-rate momentum weight-decay]} train-opts
        forward-values (forward-pass net input-vector)
        deltas-map (backward net output-vector forward-values)]
    (into {} (for [[[node-from node-to :as e] weight] (:edges net)]
               (let [delta-weight (+ (* -1 learning-rate
                                        (deltas-map node-to)
                                        (forward-values node-from))
                                     (* momentum (get prev-delta-weights e 0))
                                     (* weight-decay weight))]
                 [e delta-weight])))))

(defn backprop-step
  "Performs forward & backward pass and tunes network weights.
  Returns new network and weights deltas being made."
  [net data-vector train-opts prev-delta-weights]
  (let [delta-weights (backprop-delta-weights net data-vector train-opts prev-delta-weights)
        new-net (update-in net [:edges] #(merge-with + % delta-weights))]
    [new-net delta-weights]))

(defn train
  "Trains given network on dataset with given opts, returns new net"
  [net dataset train-opts]
  (loop [net net
         dataset-left (apply concat (repeat (:iter-count train-opts) dataset))
         delta-weights nil]
    (if (empty? dataset-left)
      net
      (let [data-vector (first dataset-left)
            [new-net new-delta-weights] (backprop-step net data-vector train-opts delta-weights)]
       (recur new-net
              (rest dataset-left)
              new-delta-weights)))))

(defn activate
  "Returns output vector of after passing given input vector on net's inputs"
  [net input-vector]
  (let [nodes-values (forward-pass net input-vector)]
    (mapv nodes-values (:nodes (last (:layers net))))))

(defn calc-error-on-vector
  "Calculates error on given data vector for given network"
  [net [input-vector target-output]]
  (let [predicted-output (activate net input-vector)]
    (/ (sum-of-squares (map - target-output predicted-output))
       2)))

(defn calc-error
  "Calculates error on given dataset for given network"
  [net dataset]
  (sum (map #(calc-error-on-vector net %) dataset)))

(defn calc-mean-error
  "Returns mean error & variance after training given net @times times"
  [net dataset times train-opts]
  {:pre [(instance? TrainOpts train-opts)]}
  (let [net-errors (for [_ (range times)]
                     (-> (network/reset-weights net)
                         (train dataset train-opts)
                         (calc-error dataset)))]
    [(mean net-errors)
     (variance net-errors)]))
