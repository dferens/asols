(ns asols.client.stats
  (:require [om.core :as om]
            [om.dom :refer [render-to-str]]
            [om-tools.core :refer-macros [defcomponent]]
            [sablono.core :refer-macros [html]]
            [asols.client.solvings :refer [solving-block mutation-view]]
            [asols.client.widgets :as widgets]
            [asols.client.utils :refer [debug format]]))

(defn- format-cost [val]
  (js/parseFloat (format "%.4f" val)))

(defn- costs-chart-config
  [solvings]
  {:chart       {:type   "line"
                 :height "400"}
   :title       {:text "Cost"}
   :xAxis       {:minorTickInterval 1
                 :tickInterval      1
                 :categories        (for [i (range (count solvings))]
                                      (format "Solving %d" (inc i)))}
   :yAxis       {:title      {:text "Cost"}
                 :maxPadding 0}
   :plotOptions {:line {:dataLabels          {:enabled true}
                        :enableMouseTracking false}}
   :tooltip     {:shared       true
                 :useHTML      true
                 :headerFormat "<b>{point.key}</b><br/>"
                 :pointFormat "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: <b>{point.y}</b><br/>"}
   :series      [{:name "Train cost"
                  :data (for [s solvings]
                          (format-cost (:train-cost (:best-case s))))}
                 {:name "Test cost"
                  :data (for [s solvings]
                          (format-cost (:test-cost (:best-case s))))}]})

(defn- format-ca [val]
  (js/parseFloat (format "%.2f" (* 100 val))))

(defn- ca-chart-config
  [solvings]
  {:chart       {:type   "line"
                 :height "500"}
   :title       {:text "CA"}
   :xAxis       {:categories        (for [i (range (count solvings))]
                                      (format "Solving %d" (inc i)))}
   :yAxis       {:title      {:text "CA, %"}
                 :maxPadding 0
                 :tickInterval 1.0}
   :plotOptions {:line {:dataLabels          {:enabled true}
                        :enableMouseTracking false}}
   :tooltip     {:shared       true
                 :useHTML      true
                 :headerFormat "<b>{point.key}</b><br/>"
                 :pointFormat "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: <b>{point.y}</b><br/>"}
   :series      [{:name "Train CA"
                  :data (for [s solvings]
                          (format-ca (:train-metrics (:best-case s))))}
                 {:name "Test CA"
                  :data (for [s solvings]
                          (format-ca (:test-metrics (:best-case s))))}]})

(defcomponent cost-chart [{solvings :solvings}]
  (render [_]
    (om/build widgets/highchart (costs-chart-config solvings))))

(defcomponent ca-chart [{solvings :solvings}]
  (render [_]
    (om/build widgets/highchart (ca-chart-config solvings))))

(defcomponent stats-panel [{:keys [progress solvings settings]}]
  (render [_]
    (let [ca-enabled? (= (:mode (:mutation-opts settings)) :asols.commands/classification)]
      (html
       [:.panel.panel-success
        [:.panel-heading "Stats"]
        [:.panel-body
         (when progress
           [:.row-fluid
            [:.col-sm-12
             [:p "Current mutation: " [:b (mutation-view (:mutation progress))]]
             (widgets/progress-bar (:value progress))]])

         [:.row
          [:.col-md-12
           (om/build cost-chart {:solvings solvings})]]

         [:.row
          (when ca-enabled?
            [:.col-md-12
             (om/build ca-chart {:solvings solvings})])]]]))))