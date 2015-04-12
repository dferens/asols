(ns asols.trainer
  (:require [clojure.core.matrix.stats :as stats]
            [asols.network :as network]))

(defn- activation-fn
  "Sigmoid activation function"
  [x]
  (/ 1 (+ 1 (Math/exp (- x)))))

(defn- calc-node-value
  "Calculates node output value"
  [net node nodes-values]
  (activation-fn
    (reduce +
      (map
        (fn [[node-from _ :as edge]]
          (* (nodes-values node-from)
             (network/get-weight net edge)))
        (network/in-edges net node)))))

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
        (* predicted
           (- 1 predicted)
           (- actual predicted))))
    (:output-layer net)))

(defn- calc-node-deltas
  [deltas net nodes-values node]
  (let [predicted (nodes-values node)
        out-edges (network/out-edges net node)]
    (* predicted
       (- 1 predicted)
       (reduce +
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
  [net nodes-values deltas {:keys [learning-rate]}]
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
                    delta-weight (* gradient learning-rate)
                    new-weight (+ weight delta-weight)]]
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
        predicted-output (mapv nodes-values (:output-layer net))
        square (fn [x] (* x x))]
    (* 0.5
      (reduce +
        (map (comp square -) expected-output predicted-output)))))

(defn- calc-error
  "Returns total error on given dataset for given network"
  [net dataset]
  (reduce + (map #(calc-error-on-vector net %) dataset)))


(defn calc-mean-error
  "Returns mean error & variance on given net"
  [net dataset times
   {:keys [learning-rate momentum iter-count]
    :or {learning-rate 0.1
         momentum 0.9
         iter-count 1000}
    :as opts}]
  (let [net-errors (into {} (for [_ (range times)
                                  :let [new-net (-> (network/reset-weights net)
                                                    (train dataset opts))
                                        new-error (calc-error new-net dataset)]]
                               [new-error new-net]))]
    {:mean-error (stats/mean (keys net-errors))
     :variance (stats/variance (keys net-errors))}))