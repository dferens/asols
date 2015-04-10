(ns asols.mutations
  (:require [asols.network :as network]))

(def operations
  #{:add-neuron
    :add-edge :del-edge})


(defn add-neurons-mutations
  "Return mutations for adding new neurons to existing hidden layers
  with 1 random edge with new neuron"
  [net]
  (for [index (range (count (:hidden-layers net)))]
    (let [[net new-node] (network/add-node net index)
          prev-layer (nth (network/layers net) index)
          next-layer (nth (network/layers net) (+ index 2))]
      {:operation :add-neuron
       :network (-> net
                    (network/add-edge (rand-nth prev-layer) new-node)
                    (network/add-edge new-node (rand-nth next-layer)))
       :added-neuron new-node})))

(defn- add-edges-mutations
  "Return mutations for adding all missing edges"
  [net]
  (let [layers (network/layers net)
        layers-pairs (map vector (butlast layers) (rest layers))]
    (for [[layer-a layer-b] layers-pairs
          node-from layer-a
          node-to layer-b
          :when (not (network/has-edge? net node-from node-to))]
      {:operation :add-edge
       :network (network/add-edge net node-from node-to)
       :added-edge [node-from node-to]})))

(defn- remove-edges-mutations
  "Return mutations for deletion of all net edges"
  [net]
  (for [[edge _] (:edges net)]
    {:operation :del-edge
     :network (network/del-edge net edge)
     :deleted-edge edge}))

(defn get-mutations
  "Returns lazy sequence of all possible network mutations like:
   [[:added-node #<Network 1>]
    [:added-layer #<Network 2>] ... ]"
  [net]
  (concat
    (add-neurons-mutations net)
    (add-edges-mutations net)
    #_(remove-edges-mutations net)))