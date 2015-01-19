(ns asols.network
  (:require [asols.utils :refer [convert-base]]))


(def ^:private alphabet (map char (range 97 123)))
(def ^:private last-node-id (atom -1))

(defn- rand-weight []
  (dec (rand 2)))

(defn- rand-weight [] (rand))

(defn- node
  "Creates new node:
    :a, :b, ..., :z, :ba, :bb, ..., :zz, :baa etc."
  []
  (let [last-id (swap! last-node-id inc)]
    (->> (convert-base last-id (count alphabet))
         (map (partial nth alphabet))
         (apply str)
         (keyword))))


(defn- edge-layer
  []
  [])

(defn- hidden-layer
  "Hidden layer is simply set of nodes"
  []
  #{})

(defn network
  "Creates new network"
  [input-count output-count]
  {:input-layer (into (edge-layer) (repeatedly input-count node))
   :output-layer (into (edge-layer) (repeatedly output-count node))
   :hidden-layers []})

(defn layers
  "Returns collection of network layers where first one is input and last one
  is output layers"
  [network]
  (concat
    [(:input-layer network)]
    (:hidden-layers network)
    [(:output-layer network)]))

(defn input-layer [network] (:input-layer network))
(defn hidden-layers [network] (:hidden-layers network))
(defn output-layer [network] (:output-layer network))

(defn edges [network] (:edges network))

(defn in-edges
  [network node]
  (for [edge (keys (:edges network))
        :let [[_ node-to] edge]
        :when (= node-to node)]
    edge))

(defn out-edges
  [network node]
  (for [edge (keys (:edges network))
        :let [[node-from _] edge]
        :when (= node-from node)]
    edge))

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

(defn get-weight
  "Returns weight of given edge"
  [network edge]
  (get-in network [:edges edge]))

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
