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

(defcomponent highchart [config owner]
  (did-mount [_]
    (let [node (om/get-node owner)
          init-config (assoc-in config [:chart :renderTo] node)]
      (js/Highcharts.Chart. (clj->js init-config))))
  (render [_]
    (html
      [:.highchart-container {:width "100%" :height "400px"}])))

(defcomponent stats-panel [{:keys [progress]}]
  (render [_]
    (html
      [:.panel.panel-success
       [:.panel-heading "Stats"]
       [:.panel-body
        (when progress
          [:.row-fluid
           [:.col-sm-12
            [:p "Current mutation: " [:b (mutation-view (:mutation progress))]]
            (widgets/progress-bar (:value progress))]])

        (om/build highchart {:chart {:type :bar}
                             :title {:text "Test"}
                             :xAxis {:categories ["A" "B" "C"]}
                             :yAxis {:title {:text "Result"}}
                             :series [{:name "One"
                                       :data [1 0 4]}
                                      {:name "Two"
                                       :data [5 7 3]}]})]])))