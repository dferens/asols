(ns asols.mutations
  (:require [asols.network :as network]
            [clojure.core.matrix :as m]))

(def operations
  #{::identity
    ::add-node ::del-node
    ::add-edge ::del-edge
    ::add-layer})

(defn- node-name
  [layer-i node-i]
  (format "%d[%d]" layer-i node-i))

(defn identity-mutations
  [net]
  [{:operation ::identity
    :network net}])

(defn add-node-mutations
  "Return mutations for adding new neurons to existing hidden layers.
  Each new neuron will be fully connected to prev & next layers."
  [net]
  (for [[i layer] (map-indexed vector (butlast (:layers net)))
        :let [layer-i (inc i)
              [_ out-count] (network/get-layer-shape layer)]]
    {:operation  ::add-node
     :network    (network/add-node net layer-i)
     :added-node (node-name layer-i out-count)}))

(defn- add-layer-edges-mutations
  [net [layer-i layer]]
  (let [[in-count out-count] (network/get-layer-shape layer)]
    (for [node-from (range in-count)
          node-to (range out-count)
          :when (not (network/edge-exists? net layer-i node-from node-to))]
      {:operation  ::add-edge
       :network    (network/add-edge net layer-i node-from node-to)
       :added-edge [(node-name layer-i node-from)
                    (node-name (inc layer-i) node-to)]})))

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
     :network      (network/del-node net (inc i) node-i)
     :deleted-node (node-name (inc i) node-i)}))

(defn- del-layer-edges-mutations
  [net [layer-i layer]]
  (let [[in-count out-count] (network/get-layer-shape layer)]
    (for [node-from (range in-count)
          node-to (range out-count)
          :when (network/edge-exists? net layer-i node-from node-to)]
      {:operation ::del-edge
       :network (network/del-edge net layer-i node-from node-to)
       :deleted-edge [(node-name layer-i node-from)
                      (node-name (inc layer-i) node-to)]})))

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
              [in-count out-count] (network/get-layer-shape layer)]]
    {:operation   ::add-layer
     :layer-type  layer-type
     :layer-index layer-i
     :network     (network/insert-layer net layer-i layer-type out-count)}))