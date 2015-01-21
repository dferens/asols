(ns asols.network
  (:require [asols.utils :refer [convert-base]]))

(def ^:private alphabet (map char (range 97 123)))
(def ^:private last-node-id (atom -1))

(defn- node
  "Creates new node:
    :a, :b, ..., :z, :ba, :bb, ..., :zz, :baa etc."
  []
  (let [last-id (swap! last-node-id inc)]
    (->> (convert-base last-id (count alphabet))
         (map (partial nth alphabet))
         (apply str)
         (keyword))))

(defn- rand-weight [] (rand))

(defn- edge-layer
  []
  [])

(defn- hidden-layer
  "Hidden layer is simply set of nodes"
  []
  #{})

(defprotocol NetworkProtocol
  (layers
    [this]
    "Returns collection of network layers where first one is input and last one
    is output layers")
  (in-edges [this node])
  (out-edges [this node])
  (has-edge? [this node-from node-to])
  (get-weight
    [this edge]
    "Returns weight of given edge"))

(defrecord Network [input-layer output-layer hidden-layers edges]
  NetworkProtocol
  (layers [_]
    (concat
      [input-layer]
      hidden-layers
      [output-layer]))
  (in-edges [_ node]
    (for [edge (keys edges)
          :let [[_ node-to] edge]
          :when (= node-to node)]
      edge))
  (out-edges [_ node]
    (for [edge (keys edges)
          :let [[node-from _] edge]
          :when (= node-from node)]
      edge))
  (has-edge? [_ node-from node-to]
    (contains? edges [node-from node-to]))
  (get-weight [_ edge]
    (edges edge)))

(defn network
  "Creates new network"
  [input-count output-count]
  (->Network
    (into (edge-layer) (repeatedly input-count node))
    (into (edge-layer) (repeatedly output-count node))
    []
    {}))

(defn add-layer
  "Adds new hidden layer to network, returns new network"
  [network]
  (update-in network [:hidden-layers] conj (hidden-layer)))

(defn add-node
  "Adds new node to `layer-i`th hidden layer of network, returns new network"
  [network layer-i]
  (update-in network [:hidden-layers layer-i] conj (node)))

(defn del-node
  "Removes given node from network, returns new network"
  [network layer-i node]
  (update-in network [:hidden-layers layer-i] dissoc node))

(defn add-edge
  "Adds new edge to network, returns new network"
  [network node-from node-to]
  (update-in network [:edges] assoc [node-from node-to] (rand-weight)))

(defn set-weight
  "Sets new weight for given edge, returns new network"
  [network edge weight]
  (update-in network [:edges] assoc edge weight))

(defn del-edge
  "Removes edge from network, returns new network"
  [network edge]
  (update-in network [:edges] dissoc edge))

(defn reset-weights
  [network]
  (let [new-edges (for [[k _] (:edges network)]
                    [k (rand-weight)])]
    (assoc network :edges (into {} new-edges))))
