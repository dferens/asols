(ns asols.graphics
  (:require [dorothy.core :as dot]
            [asols.network :as network]))

(defn net->dot
  [net]
  (let [layers (network/layers net)
        params [{:rankdir "LR"}]
        graphs (map-indexed
                 (fn [i layer]
                   (let [layer-id (keyword (str "cluster_" i))
                         [label color] (condp = layer
                                         (:input-layer net) ["input layer" :red]
                                         (:output-layer net) ["output layer" :blue]
                                         ["hidden layers" :green])]
                     (dot/subgraph layer-id
                       [{:label label :color "white"}
                        (dot/node-attrs {:style "solid" :shape "circle" :color color})
                        (map vector layer)])))
                 layers)
        edges (for [[edge weight] (:edges net)]
                edge #_(into edge [{:label (format "%.2f" weight)}]))]
    (dot/dot (dot/digraph (concat params graphs edges)))))


(defn render-network
  "Render given network, return result as string"
  [net & {:keys [format] :or {format :svg}}]
  (-> (net->dot net)
      (dot/render {:format format})))

(defn display-network
  [net]
  (-> (net->dot net)
      (dot/show!)))