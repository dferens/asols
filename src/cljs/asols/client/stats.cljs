(ns asols.client.stats
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [sablono.core :refer-macros [html]]
            [asols.client.solvings :refer [solving-block mutation-view]]
            [asols.client.widgets :as widgets]))

(defcomponent failed-solving-panel [solving]
  (render [_]
    (html
      [:.panel.panel-info
       [:.panel-heading "Further tryings:"]
       [:ul.list-group
        (om/build solving-block {:solving solving :visible? true})]])))

(defcomponent stats-panel [{:keys [progress]}]
  (render [_]
    (html
      [:.panel.panel-success
       [:.panel-heading "Stats"]
       (when progress
         [:.panel-body
          [:.row-fluid
           [:.col-sm-12
            [:p "Current mutation: " [:b (mutation-view (:mutation progress))]]
            (widgets/progress-bar (:value progress))]]])])))