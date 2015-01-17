(ns asols.network
  (:require [asols.utils :refer [convert-base]]))


(def ^:private alphabet (map char (range 97 123)))
(def ^:private last-node-id (atom -1))

(defn node
  "Creates new node:
    :a, :b, ..., :z, :ba, :bb, ..., :zz, :baa etc."
  []
  (let [last-id (swap! last-node-id inc)]
    (->> (convert-base last-id (count alphabet))
         (map (partial nth alphabet))
         (apply str)
         (keyword))))

(defn- layer
  "Creates new network layer"
  []
  #{})

(defn network
  "Creates new network"
  []
  {:layers []
   :edges {}})

(defn add-layer
  "Adds new hidden layer to network, returns new network"
  [network]
  (update-in network [:layers] conj (layer)))

(defn add-node
  "Adds new node to `layer-i`th layer of network, returns new network"
  [network layer-i]
  (update-in network [:layers layer-i] conj (node)))

(defn del-node
  "Removes given node from network, returns new network"
  [network layer-i node]
  (update-in network [:layers layer-i] dissoc node))

(defn add-edge
  "Adds new edge to network, returns new network"
  [network node-from node-to]
  (update-in network [:edges] assoc [node-from node-to] 1.0))

(defn get-weight
  "Returns weight of given edge"
  [network node-from node-to]
  (get-in network [:edges [node-from node-to]]))

(defn set-weight
  "Sets new weight for given edge, returns new network"
  [network node-from node-to weight]
  (update-in network [:edges] assoc [node-from node-to] weight))

(defn del-edge
  "Removes edge from network, returns new network"
  [network edge]
  (update-in network [:edges] dissoc edge))