(ns asols.graphics
  (:require [dorothy.core :as dot]
            [asols.network :as network]))

(defn render-network
  "Render and save the given network. f is any argument acceptable to
  (clojure.java.io/output-stream)."
  [net f & {:keys [format] :or {format :svg}}]
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
                (into edge [{:label (format "%.2f" weight)}]))]
    (-> (dot/digraph (concat params graphs edges))
        (dot/dot)
        (dot/save! f {:format format}))))