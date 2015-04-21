(ns asols.mutations
  (:require [asols.network :as network]
            [asols.trainer :as trainer]))

(def operations
  #{::identity
    ::add-neuron ::del-neuron
    ::add-edge   ::del-edge
    ::add-layer})

(defn identity-mutations
  [net]
  [{:operation ::identity
    :network net}])

(defn add-neurons-mutations
  "Return mutations for adding new neurons to existing hidden layers.
  Each new neuron will be fully connected to prev & next layers."
  [net]
  (for [index (range (count (network/hidden-layers net)))]
    (let [[new-net new-node] (network/add-node net (inc index))]
      {:operation ::add-neuron
       :network (network/full-connect new-net (inc index) new-node)
       :added-neuron new-node})))

(defn add-edges-mutations
  "Return mutations for adding all missing edges"
  [net]
  (let [layers (:layers net)
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
  (let [hidden-layers (network/hidden-layers net)
        hidden-layers-indexed (map-indexed (fn [i v] [i v]) hidden-layers)]
    (for [[hidden-layer-i layer] hidden-layers-indexed
          node (:nodes layer)
          :let [layer-i (inc hidden-layer-i)]
          :when (> (count (:nodes layer)) 1)]
      {:operation ::del-neuron
       :network (network/del-node net layer-i node)
       :deleted-neuron node})))

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
  (for [layer-i (range (count (:layers net)))
        layer-type (trainer/hidden-types)
        :when (pos? layer-i)]
    (let [{:keys [nodes]} (nth (:layers net) layer-i)
          new-net (network/add-layer net layer-type layer-i)
          node-splitter #(network/split-node %1 (inc layer-i) %2)]
      {:operation ::add-layer
       :layer-type layer-type
       :layer-index layer-i
       :network (reduce node-splitter new-net nodes)})))