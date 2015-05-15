(ns asols.mutations
  (:require [asols.network :as network]
            [clojure.core.matrix :as m]))

(def operations
  #{::identity
    ::add-node ::del-node
    ::add-edge ::del-edge
    ::add-layer})

(defmulti mutate (fn [net mutation] (:operation mutation)))

(defn identity-mutations [_]
  [{:operation ::identity}])

(defmethod mutate ::identity [net _] net)

(defn add-node-mutations
  "Return mutations for adding new neurons to existing hidden layers.
  Each new neuron will be fully connected to prev & next layers."
  [net]
  (for [[i layer] (map-indexed vector (butlast (:layers net)))
        :let [layer-i (inc i)
              [_ out-count] (network/get-layer-shape layer)]]
    {:operation  ::add-node
     :added-node [layer-i out-count]}))

(defmethod mutate ::add-node
  [net {:keys [added-node]}]
  (let [[layer-i _] added-node]
    (network/add-node net layer-i)))

(defn- add-layer-edges-mutations
  [net [layer-i layer]]
  (let [[in-count out-count] (network/get-layer-shape layer)]
    (for [node-from (range in-count)
          node-to (range out-count)
          :when (not (network/edge-exists? net layer-i node-from node-to))]
      {:operation  ::add-edge
       :added-edge [[layer-i node-from]
                    [(inc layer-i) node-to]]})))

(defmethod mutate ::add-edge
  [net {:keys [added-edge]}]
  (let [[[layer-i node-from] [_ node-to]] added-edge]
    (network/add-edge net layer-i node-from node-to)))

(defn add-edge-mutations
  "Return mutations for adding all missing edges"
  [net]
  (mapcat
    #(add-layer-edges-mutations net %)
    (map-indexed vector (:layers net))))

(defn del-node-mutations
  [net]
  (for [[i {:keys [edges-matrix]}] (map-indexed vector (butlast (:layers net)))
        node-i (range (m/dimension-count edges-matrix 1))]
    {:operation    ::del-node
     :deleted-node [(inc i) node-i]}))

(defmethod mutate ::del-node
  [net {:keys [deleted-node]}]
  (let [[layer-i node-i] deleted-node]
    (network/del-node net layer-i node-i)))

(defn- del-layer-edges-mutations
  [net [layer-i layer]]
  (let [[in-count out-count] (network/get-layer-shape layer)]
    (for [node-from (range in-count)
          node-to (range out-count)
          :when (network/edge-exists? net layer-i node-from node-to)]
      {:operation ::del-edge
       :deleted-edge [[layer-i node-from]
                      [(inc layer-i) node-to]]})))

(defmethod mutate ::del-edge
  [net {:keys [deleted-edge]}]
  (let [[[layer-from node-from] [_ node-to]] deleted-edge]
    (network/del-edge net layer-from node-from node-to)))

(defn del-edge-mutations
  "Return mutations for deletion of all net edges"
  [net]
  (mapcat
    #(del-layer-edges-mutations net %)
    (map-indexed vector (:layers net))))

(defn add-layers-mutations
  "Returns mutations for adding new hidden layers to net"
  [net]
  (for [[i layer] (map-indexed vector (:layers net))
        :let [layer-i (inc i)
              layer-type (:type layer)
              [_ out-count] (network/get-layer-shape layer)]]
    {:operation  ::add-layer
     :layer-type layer-type
     :layer-pos  layer-i
     :nodes-count out-count}))

(defmethod mutate ::add-layer
  [net {:keys [layer-type layer-pos nodes-count]}]
  (network/insert-layer net layer-pos layer-type nodes-count))