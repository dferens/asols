(ns asols.graphics
  (:require [dorothy.core :as dot]))

(defn net->dot
  [net & {:keys [show-weights?]
          :or {show-weights? true}}]
  (let [graph-params [{:rankdir "LR"}]
        subgraphs (for [layer-i (range (count (:layers net)))]
                    (let [layer (nth (:layers net) layer-i)
                          [label color] (condp = layer
                                          (first (:layers net)) ["input layer" :red]
                                          (last (:layers net)) ["output layer" :blue]
                                          ["hidden layer" :green])
                          layer-id (keyword (str "cluster_" layer-i))]
                      (dot/subgraph layer-id
                                    [{:label label :color "white"}
                                     (dot/node-attrs {:style "solid" :shape "circle" :color color})
                                     (map vector (:nodes layer))])))

        edges (for [[[node-from node-to] weight] (:edges net)]
                [node-from node-to (when show-weights?
                                     {:label (format "%.2f" weight)} )])]
    (-> (concat graph-params subgraphs edges)
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
