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

(defcomponent highchart [config owner]

  (init-state [_] {:chart-obj nil})

  (did-mount [_]
    (debug "Creating chart")
    (let [node (om/get-node owner)
          init-config (assoc-in config [:chart :renderTo] node)
          chart-obj (js/Highcharts.Chart. (clj->js init-config))]
      (om/set-state! owner :chart-obj chart-obj)))

  (will-receive-props [_ next-config]
    (let [config (om/get-props owner)
          chart (om/get-state owner :chart-obj)]
      (when (not= config next-config)
        (debug "Updating chart")
        (doseq [serie-i (range (count (:series next-config)))]
          (let [new-serie (nth (:series next-config) serie-i)]
            (-> chart
                (aget "series" serie-i)
                (.setData (clj->js (:data new-serie)))))))))

  (will-unmount [_]
    (debug "Destroying chart")
    (.destroy (om/get-state owner :chart-obj)))

  (render [_]
    (html
      [:.highchart-container {:width "100%" :height "400px"}])))


(defn test-error-serie
  [solvings]
  (->> (map :best-case solvings)
       (map :test-error)))

(defn train-error-serie
  [solvings]
  (->> (map :best-case solvings)
       (map :train-error)))

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
     :series [{:name "Train error" :data (train-error-serie solvings)}
              {:name "Test error" :data (test-error-serie solvings)}]}))

(defcomponent errors-chart [{solvings :solvings}]
  (render [_]
    (om/build highchart (errors-chart-config solvings))))

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