(ns asols.trainer
  (:require [clojure.core.matrix.stats :refer [sum sum-of-squares mean variance]]
            [asols.worker]
            [asols.network :as network])
  (:import (asols.worker TrainOpts)))

(defn- act-fn
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

(defn- activate
  "Calculates nodes outputs on given data vector, returns map of values"
  [net input-vector]
  (reduce
    (fn [result layer-nodes]
      (reduce
        (fn [values node]
          (assoc values node (calc-node-value net node values)))
        result
        layer-nodes))
    (zipmap (:input-layer net) input-vector)
    (rest (network/layers net))))

(defn- calc-output-deltas
  "Calculates deltas of output layer, returns collection of deltas"
  [net nodes-values data-vector]
  (map-indexed
    (fn [node-index output-node]
      (let [predicted (nodes-values output-node)
            [_ out-vector] data-vector
            actual (nth out-vector node-index)]
        (* (act-fn-dx predicted)
           (- actual predicted))))
    (:output-layer net)))

(defn- calc-node-deltas
  [deltas net nodes-values node]
  (let [predicted (nodes-values node)
        out-edges (network/out-edges net node)]
    (* (act-fn-dx predicted)
       (sum
         (map
           (fn [[_ node-to :as edge]]
             (* (deltas node-to)
                (network/get-weight net edge)))
           out-edges)))))

(defn- calc-layer-deltas
  [deltas net nodes-values layer-nodes]
  (reduce
    (fn [deltas node]
      (let [node-delta (calc-node-deltas deltas net nodes-values node)]
        (assoc deltas node node-delta)))
    deltas
    layer-nodes))

(defn- calc-deltas
  "Calculates network deltas, returns collection of deltas"
  [net nodes-values data-vector]
  (let [output-deltas (calc-output-deltas net nodes-values data-vector)
        layers-to-process (butlast (network/layers net))]
    (reduce
      (fn [deltas layer]
        (calc-layer-deltas deltas net nodes-values layer))
      (zipmap (:output-layer net) output-deltas)
      (reverse layers-to-process))))

(defn- modify-weights
  [net nodes-values deltas opts]
  (reduce
    (fn [net layer-nodes]
      (reduce
        (fn [net [edge new-weight]]
          (network/set-weight net edge new-weight))
        net
        (for [node layer-nodes
              edge (network/in-edges net node)
              :let [node-from (first edge)
                    weight (network/get-weight net edge)
                    gradient (* (nodes-values node-from) (deltas node))
                    delta-weight (* gradient (:learning-rate opts))
                    new-weight (+ (* weight (:momentum opts)) delta-weight)]]
          [edge new-weight])))
    net
    (rest (network/layers net))))

(defn- train-on-vector
  "Trains network on given data vector, returns new network"
  [net data-vector opts]
  (let [nodes-values (activate net (first data-vector))
        deltas (calc-deltas net nodes-values data-vector)]
    (modify-weights net nodes-values deltas opts)))

(defn- train
  "Trains network on given dataset, returns new network"
  [net dataset {:keys [iter-count] :as opts}]
  (reduce
    #(train-on-vector %1 %2 opts)
    net
    (for [_ (range iter-count)
          training-example dataset]
      training-example)))

(defn- calc-error-on-vector
  "Returns error value on given data vector for given network"
  [net [input-vector expected-output]]
  (let [nodes-values (activate net input-vector)
        predicted-output (mapv nodes-values (:output-layer net))]
    (* 0.5
       (sum-of-squares (map - expected-output predicted-output)))))

(defn- calc-error
  "Returns total error on given dataset for given network"
  [net dataset]
  (sum (map #(calc-error-on-vector net %) dataset)))

(defn calc-mean-error
  "Returns mean error & variance after training given net @times times"
  [net dataset times train-opts]
  {:pre [(instance? TrainOpts train-opts)]}
  (let [net-errors (into {} (for [_ (range times)
                                  :let [new-net (-> (network/reset-weights net)
                                                    (train dataset train-opts))
                                        new-error (calc-error new-net dataset)]]
                               [new-error new-net]))]
    {:mean-error (mean (keys net-errors))
     :variance (variance (keys net-errors))}))