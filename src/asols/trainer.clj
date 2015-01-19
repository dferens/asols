(ns asols.trainer
  (:require [asols.network :as network]))


(def dataset
  [[[0 0] [0]]
   [[1 1] [0]]
   [[1 0] [1]]
   [[0 1] [1]]])

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
      (zipmap (network/input-layer net) input-vector)
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
    (network/output-layer net)))

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
      (zipmap (network/output-layer net) output-deltas)
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
    (network/edges net)))

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
  [net iterations & {:keys [learning-rate]
                     :or {learning-rate 0.1}}]
  (reduce
    #(learn-on-vector %1 %2 learning-rate)
    net
    (for [_ (range iterations)
          training-example dataset]
      training-example)))

(def test-net
  (let [net (-> (network/network 2 1)
                (network/add-layer)
                (network/add-node 0)
                (network/add-node 0)
                (network/add-node 0))
        [i1 i2] (network/input-layer net)
        [h1 h2 h3] (seq (first (network/hidden-layers net)))
        [o1] (network/output-layer net)]
    (-> net
        (network/add-edge i1 h1)
        (network/add-edge i1 h2)
        (network/add-edge i1 h3)
        (network/add-edge i2 h1)
        (network/add-edge i2 h2)
        (network/add-edge i2 h3)

        (network/add-edge h1 o1)
        (network/add-edge h2 o1)
        (network/add-edge h3 o1))))