(ns asols.client.stats
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [sablono.core :refer-macros [html]]
            [asols.client.solvings :refer [solving-block mutation-view]]
            [asols.client.widgets :as widgets]
            [asols.client.utils :refer [debug]]))

(defn test-value-serie
  [solvings]
  (->> (map :best-case solvings)
       (map :test-cost)))

(defn train-value-serie
  [solvings]
  (->> (map :best-case solvings)
       (map :train-cost)))

(defn- errors-chart-config
  [solvings]
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
            {:name "Test error" :data (test-value-serie solvings)}]})

(defcomponent errors-chart [{solvings :solvings}]
  (render [_]
    (om/build widgets/highchart (errors-chart-config solvings))))

(defcomponent stats-panel [{:keys [progress solvings]}]
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