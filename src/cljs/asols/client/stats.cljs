(ns asols.client.stats
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [sablono.core :refer-macros [html]]
            [asols.client.solvings :refer [solving-block mutation-view]]
            [asols.client.widgets :as widgets]
            [asols.client.utils :refer [debug]]))

(defcomponent failed-solving-panel [solving]
  (render [_]
    (html
      [:.panel.panel-info
       [:.panel-heading "Further tryings:"]
       [:ul.list-group
        (om/build solving-block {:solving solving :visible? true})]])))

(defn test-value-serie
  [solvings]
  (->> (map :best-case solvings)
       (map :test-value)))

(defn train-value-serie
  [solvings]
  (->> (map :best-case solvings)
       (map :train-value)))

(defn- errors-chart-config
  [solvings]
  (let []
    {:chart {:type "spline"
             :height "400"}
     :title {:text "Test & train errors"}
     :xAxis {:minorTickInterval 1
             :tickInterval 1}
     :yAxis {:title {:text "Error"}
             :type "logarithmic"
             :maxPadding 0}
     :tooltip {:headerFormat "<b>{series.name}</b><br/>"}
     :plotOptions {:line {:dataLabels {:enabled true}
                          :enableMouseTracking false}}
     :series [{:name "Train error" :data (train-value-serie solvings)}
              {:name "Test error" :data (test-value-serie solvings)}]}))

(defcomponent errors-chart [{solvings :solvings}]
  (render [_]
    (om/build widgets/highchart (errors-chart-config solvings))))

(defcomponent stats-panel [{:keys [settings progress solvings]}]
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

        (om/build errors-chart {:solvings solvings})]])))