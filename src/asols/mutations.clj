(ns asols.mutations
  (:require [asols.network :as network]))

(def operations
  #{:add-edge :del-edge})


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
    (add-edges-mutations net)
    (remove-edges-mutations net)))