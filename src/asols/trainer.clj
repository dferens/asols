(ns asols.trainer
  (:require [asols.network :as network]))


(defn get-test-error
  "Returns error value on test data for given network"
  [network]
  1.0)

(defn activation-fn
  "Sigmoid activation function"
  [x]
  (/ 1 (+ 1 (Math/exp (- x)))))

(defn calc-node-value
  "Calculates node output value"
  [net node nodes-values]
  (activation-fn
    (apply +
      (map
        (fn [[node-from _ :as edge]]
          (* (nodes-values node-from)
             (network/get-weight net edge)))
        (network/in-edges net node)))))

(defn activate
  "Calculates nodes outputs on given data vector, returns map of values"
  [net data-vector]
  (let [[input-vector _] data-vector]
    (reduce
      (fn [result layer-nodes]
        (reduce
          (fn [values node]
            (assoc values node (calc-node-value net node values)))
          result
          layer-nodes))
      (zipmap (:input-layer net) input-vector)
      (rest (network/layers net)))))

(defn- calc-output-deltas
  "Calculates deltas of output layer, returns collection of deltas"
  [net nodes-values data-vector]
  (map-indexed
    (fn [node-index output-node]
      (let [predicted (nodes-values output-node)
            [_ out-vector] data-vector
            actual (nth out-vector node-index)]
        (* (* -1 predicted)
           (- 1 predicted)
           (- actual predicted))))
    (:output-layer net)))

(defn calc-node-deltas
  [deltas net nodes-values node]
  (let [predicted (nodes-values node)
        out-edges (network/out-edges net node)]
    (* predicted
       (- 1 predicted)
       (apply +
         (map
           (fn [[_ node-to :as edge]]
             (* (deltas node-to)
                (network/get-weight net edge)))
           out-edges)))))

(defn calc-layer-deltas
  [deltas net nodes-values layer-nodes]
  (reduce
    (fn [deltas node]
      (let [node-delta (calc-node-deltas deltas net nodes-values node)]
        (assoc deltas node node-delta)))
    deltas
    layer-nodes))

(defn calc-deltas
  "Calculates network deltas, returns collection of deltas"
  [net nodes-values data-vector]
  (let [output-deltas (calc-output-deltas net nodes-values data-vector)
        layers-to-process (butlast (network/layers net))]
    (reduce
      (fn [deltas layer]
        (calc-layer-deltas deltas net nodes-values layer))
      (zipmap (:output-layer net) output-deltas)
      (reverse layers-to-process))))

(defn modify-weights
  [net nodes-values deltas learning-rate]
  (reduce
    (fn [net [[node-from node-to :as edge] weight]]
      (let [gradient (* (nodes-values node-from) (deltas node-to))
            delta-weight (* gradient learning-rate)
            new-weight (- weight delta-weight)]
        (network/set-weight net edge new-weight)))
    net
    (:edges net)))

(defn learn-on-vector
  "Learns network on given data vector, returns new network"
  [net data-vector learning-rate]
  (let [nodes-values (activate net data-vector)
        deltas (calc-deltas net nodes-values data-vector)]
    (modify-weights net nodes-values deltas learning-rate)))

(defn learn
  "Learns network on given dataset.

   Keyword arguments:
     :learning-rate (0...1) - how fast network trains"
  [net dataset iterations & {:keys [learning-rate]
                             :or {learning-rate 0.1}}]
  (reduce
    #(learn-on-vector %1 %2 learning-rate)
    net
    (for [_ (range iterations)
          training-example dataset]
      training-example)))
