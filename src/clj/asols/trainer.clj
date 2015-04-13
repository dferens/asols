(ns asols.trainer
  (:require [clojure.core.matrix.stats :refer [sum sum-of-squares mean variance]]
            [asols.worker]
            [asols.network :as network])
  (:import (asols.worker TrainOpts)))

(defn act-fn
  "Sigmoid activation function"
  [x]
  (/ 1 (+ 1 (Math/exp (- x)))))

(defn- act-fn-dx
  "Sigmoid activation function derivative"
  [fx]
  (* fx (- 1 fx)))

(defn- calc-node-value
  "Calculates node output value"
  [net node nodes-values]
  (act-fn
    (sum
      (for [[node-from _ :as edge] (network/in-edges net node)]
        (* (nodes-values node-from)
           (network/get-weight net edge))))))

(defn- forward-layer
  [net nodes-values layer]
  (into nodes-values (for [node layer
                           :let [node-out (calc-node-value net node nodes-values)]]
                       [node node-out])))

(defn forward
  "Performs forward pass, returns map of nodes outputs"
  [net input-vector]
  (let [nodes-values (zipmap (:input-layer net) input-vector)
        layers (rest (network/layers net))
        layer-reducer #(forward-layer net %1 %2)]
    (reduce layer-reducer nodes-values layers)))

(defn- backward-out-layer
  "Backward pass for output layer"
  [layer forward-values target-vector]
  (let [target-values (zipmap layer target-vector)]
    (into {} (for [node layer]
               (let [node-out (forward-values node)
                     node-target (target-values node)
                     delta (* (act-fn-dx node-out) (- node-out node-target))]
                 [node delta])))))

(defn- backward-layer
  "Backward pass for hidden & input layers"
  [net forward-values deltas-map layer]
  (into deltas-map (for [node layer]
                     (let [node-out (forward-values node)
                           out-edges (network/out-edges net node)
                           delta (* (act-fn-dx node-out)
                                    (sum (for [[_ node-to :as e] out-edges]
                                           (* (deltas-map node-to)
                                              (network/get-weight net e)))))]
                       [node delta]))))

(defn backward
  "Performs backward pass, returns map of nodes deltas"
  [net target-vector forward-values]
  (let [out-layer (:output-layer net)
        layers (reverse (butlast (network/layers net)))
        deltas-map (backward-out-layer out-layer forward-values target-vector)
        layer-reducer #(backward-layer net forward-values %1 %2)]
    (reduce layer-reducer deltas-map layers)))

(defn- backprop-delta-weights
  "Performs forward & backward pass, returns map of delta weights of edges"
  [net [input-vector output-vector] train-opts prev-delta-weights]
  (let [{:keys [learning-rate momentum weight-decay]} train-opts
        forward-values (forward net input-vector)
        deltas-map (backward net output-vector forward-values)]
    (into {} (for [[[node-from node-to :as e] weight] (:edges net)]
               (let [delta-weight (+ (* -1 learning-rate (deltas-map node-to) (forward-values node-from))
                                     (* momentum (get prev-delta-weights e 0))
                                     (* weight-decay weight))]
                 [e delta-weight])))))

(defn backprop-step
  "Trains given network on @data-vector returns network with updated
  weights & map of weights deltas"
  [net data-vector train-opts prev-delta-weights]
  (let [delta-weights (backprop-delta-weights net data-vector train-opts prev-delta-weights)
        new-net (update-in net [:edges] #(merge-with + % delta-weights))]
    [new-net delta-weights]))

(defn train
  "Trains given network on dataset with given opts, returns new net"
  [net dataset {iter-count :iter-count :as train-opts}]
  (loop [net net
         dataset-left (apply concat (repeat iter-count dataset))
         delta-weights nil]
    (if (empty? dataset-left)
      net
      (let [data-vector (first dataset-left)
            [new-net new-delta-weights] (backprop-step net data-vector train-opts delta-weights)]
       (recur new-net
              (rest dataset-left)
              new-delta-weights)))))

(defn calc-error-on-vector
  "Calculates error on given data vector for given network"
  [net [input-vector target-output]]
  (let [nodes-values (forward net input-vector)
        predicted-output (mapv nodes-values (:output-layer net))]
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
    {:mean-error (mean net-errors)
     :variance (variance net-errors)}))

(defn activate
  "Returns output vector of after passing given input vector on net's inputs"
  [net input-vector]
  (let [nodes-values (forward net input-vector)]
    (vec
      (for [node (:output-layer net)]
        (nodes-values node)))))