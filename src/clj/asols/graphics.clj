(ns asols.graphics
  (:require [clojure.core.matrix :as m]
            [dorothy.core :as dot]
            [asols.network :as network]))

(def ^:private def-graph-opts
  {:rankdir "LR"
   :ranksep 1
   :nodesep 0.1})

(def ^:private def-edge-opts
  {:penwidth      0.25
   :labelfontsize "1"})

(def ^:private max-edges-to-display 20)

(defn- node->name
  [layer-i node-i]
  (keyword (format "%d-%d" layer-i node-i)))

(defn- layer->subgraph
  [layer-i label color nodes-count]
  (dot/subgraph
    (keyword (str "cluster_" layer-i))
    [{:label label :color "white"}
     (dot/node-attrs {:style "solid"
                      :shape "circle"
                      :color color})
     (for [node-i (range nodes-count)]
       [(node->name layer-i node-i)])]))

(defn- net->dot*
  [{:keys [layers] :as net}]
  (let [graph-opts [def-graph-opts]
        layers-count (inc (count layers))
        [in-count out-count] (network/shape net)
        in-layer (layer->subgraph 0 "input layer" :red in-count)
        out-layer (layer->subgraph (dec layers-count) "output layer" :blue out-count)
        hidden-layers (for [[i layer] (map-indexed vector (butlast layers))
                            :let [[_ out-count] (network/get-layer-shape layer)]]
                        (layer->subgraph (inc i) "hidden layer" :green out-count))
        edges (for [[i layer] (map-indexed vector layers)
                    :let [layer-i (inc i)
                          [in-count out-count] (network/get-layer-shape layer)
                          show-weights? (< in-count max-edges-to-display)]]
                (for [node-from (range in-count)
                      node-to (range out-count)
                      :let [node-from-name (node->name (dec layer-i) node-from)
                            node-to-name (node->name layer-i node-to)
                            weight (network/get-layer-edge layer node-from node-to)
                            opts def-edge-opts
                            opts (if show-weights?
                                   (merge opts {:label (format "%.2f" (double weight))})
                                   opts)]
                      :when (network/edge-exists? net (dec layer-i) node-from node-to)]
                  [node-from-name node-to-name opts]))
        subgraphs (concat [in-layer] hidden-layers [out-layer])]
    (-> (concat graph-opts subgraphs (apply concat edges))
        (dot/digraph)
        (dot/dot))))

(defn net->dot
  [{:keys [layers] :as net}]
  (let [in-counts (for [layer layers]
                    (first (network/get-layer-shape layer)))
        should-render? (< (apply max in-counts) 10)]
    (if should-render?
      (net->dot* net)
      "")))

(defn render-network
  "Render given network, return result as string"
  [net & {:keys [format] :or {format :svg}}]
  (-> (net->dot net)
      (dot/render {:format format})))

(defn display-network
  [net]
  (-> (net->dot net)
      (dot/show!)))
