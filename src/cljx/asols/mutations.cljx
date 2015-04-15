(ns asols.mutations
  (:require [asols.network :as network]))

(def operations
  #{::add-neuron ::del-neuron
    ::add-edge   ::del-edge
    ::add-layer})

(defn add-neurons-mutations
  "Return mutations for adding new neurons to existing hidden layers.
  Each new neuron will be fully connected to prev & next layers."
  [net]
  (for [index (range (count (:hidden-layers net)))]
    (let [prev-layer (nth (network/layers net) index)
          next-layer (nth (network/layers net) (+ index 2))
          [new-net new-node] (network/add-node net index)]
      {:operation    ::add-neuron
       :network      (as-> new-net $
                           (reduce #(network/add-edge %1 %2 new-node) $ (:nodes prev-layer))
                           (reduce #(network/add-edge %1 new-node %2) $ (:nodes next-layer)))
       :added-neuron new-node})))

(defn add-edges-mutations
  "Return mutations for adding all missing edges"
  [net]
  (let [layers (network/layers net)
        layers-pairs (map vector (butlast layers) (rest layers))]
    (for [[layer-a layer-b] layers-pairs
          node-from (:nodes layer-a)
          node-to (:nodes layer-b)
          :when (not (network/has-edge? net node-from node-to))]
      {:operation ::add-edge
       :network (network/add-edge net node-from node-to)
       :added-edge [node-from node-to]})))

(defn remove-neurons-mutations
  [net]
  (let [layers (:hidden-layers net)
        layers-indexed (for [i (range (count layers))]
                         [i (nth layers i)])]
    (for [[layer-i layer] layers-indexed
          node (:nodes layer)
          :when (> (count (:nodes layer)) 1)]
      (let []
        {:operation      ::del-neuron
         :network        (network/del-node net layer-i node)
         :deleted-neuron node}))))

(defn remove-edges-mutations
  "Return mutations for deletion of all net edges"
  [net]
  (for [[edge _] (:edges net)
        :when (not (network/single-edge? net edge))]
    {:operation ::del-edge
     :network (network/del-edge net edge)
     :deleted-edge edge}))

(defn add-layers-mutations
  "Returns mutations for adding new hidden layers to net"
  [net]
  (let [next-layer-i (count (:hidden-layers net))
        split-layer (nth (network/layers net) next-layer-i)
        node-splitter #(network/split-node %1 %2 (dec next-layer-i))
        type (:type (first (:hidden-layers net)))]
    [{:operation ::add-layer
      :network   (as-> net $
                       (network/add-layer $ (dec next-layer-i) type)
                       (reduce node-splitter $ (:nodes split-layer)))}]))