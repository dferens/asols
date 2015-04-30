(ns asols.graphics
  (:require [dorothy.core :as dot]))

(def ^:private def-graph-opts
  {:rankdir "LR"
   :ranksep 1
   :nodesep 0.1})

(def ^:private def-edge-opts
  {:penwidth      0.25
   :labelfontsize "1"})

(def ^:private max-edges-to-display 20)

(defn- layer->subgraph
  [layer-id label color nodes]
  (dot/subgraph layer-id
                [{:label label :color "white"}
                 (dot/node-attrs {:style "solid" :shape "circle" :color color})
                 (map vector nodes)]))

(defn- layer-edges
  [layer edges]
  (let [layer-nodes (set (:nodes layer))
        layer-in-edges (filter (fn [[[_ n] _]] (layer-nodes n)) edges)
        show-weights? (< (count layer-in-edges) max-edges-to-display)]
    (for [[[node-from node-to] weight] layer-in-edges
          :let [opts def-edge-opts
                opts (if show-weights?
                       (merge opts {:label (format "%.2f" weight)})
                       opts)]]
      [node-from node-to opts])))

(defn net->dot
  [{:keys [layers edges] :as net}]
  (let [graph-opts [def-graph-opts]
        layers-indexed (map-indexed vector layers)
        subgraphs (for [[layer-i layer] layers-indexed]
                    (let [[label color] (condp = layer
                                          (first layers) ["input layer" :red]
                                          (last layers) ["output layer" :blue]
                                          ["hidden layer" :green])
                          layer-id (keyword (str "cluster_" layer-i))]
                      (layer->subgraph layer-id label color (:nodes layer))))

        edges (mapcat #(layer-edges % edges) layers)]
    (-> (concat graph-opts subgraphs edges)
        (dot/digraph)
        (dot/dot))))

(defn render-network
  "Render given network, return result as string"
  [net & {:keys [format] :or {format :svg}}]
  (-> (net->dot net)
      (dot/render {:format format})))

(defn display-network
  [net]
  (-> (net->dot net)
      (dot/show!)))
