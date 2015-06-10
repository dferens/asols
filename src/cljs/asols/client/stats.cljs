(ns asols.client.stats
  (:require [clojure.string :as str]
            [om.core :as om]
            [om.dom :refer [render-to-str]]
            [om-tools.core :refer-macros [defcomponent]]
            [sablono.core :refer-macros [html]]
            [asols.client.solvings :refer [solving-block mutation-view]]
            [asols.client.widgets :as widgets]
            [asols.client.utils :refer [debug format log]]))

(defn- iter-count-serie
  [solvings]
  (let [iter-counts (for [{t-opts :train-opts} solvings]
                      (:iter-count t-opts))]
    (rest
      (reduce
        (fn [result val]
          (conj result (+ val (last result))))
        [0]
        iter-counts))))

(defn- solvings->serie
  [solvings val-fn]
  (let [iter-counts (iter-count-serie solvings)]
    (for [[solving x] (map vector solvings iter-counts)
         :let [y (val-fn solving)]]
     [x y])))

(defn- costs-chart-config
  [train-serie test-serie static-train-serie static-test-serie]
  {:chart       {:type   "scatter"
                 :height "400"}
   :title       {:text "Cost"}
   :xAxis       {:allowDecimals false}
   :yAxis       {:title      {:text "Cost"}
                 :maxPadding 0}
   :plotOptions {:scatter {:lineWidth 1
                           :marker    {:enabled false}}}
   :tooltip     {:shared       true
                 :useHTML      true
                 :headerFormat "<b>{point.key}</b><br/>"
                 :pointFormat  "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: <b>{point.y}</b><br/>"}
   :series      [{:name             "Static train cost"
                  :data             static-train-serie
                  :color            "red"
                  :allowPointSelect true}
                 {:name             "Static test cost"
                  :data             static-test-serie
                  :color            "red"
                  :dashStyle        "Dash"
                  :allowPointSelect true}
                 {:name   "Train cost"
                  :data   train-serie
                  :color  "blue"
                  :marker {:enabled true :symbol "square"}}
                 {:name      "Test cost"
                  :data      test-serie
                  :color     "blue"
                  :dashStyle "Dash"
                  :marker    {:enabled true :symbol "square"}}]})

(defn- ca-chart-config
  [train-serie test-serie static-train-serie static-test-serie]
  {:chart       {:type   "scatter"
                 :height "400"}
   :title       {:text "CA"}
   :xAxis       {:allowDecimals false}
   :yAxis       {:title      {:text "CA"}
                 :maxPadding 0}
   :plotOptions {:scatter {:lineWidth 1
                           :marker    {:enabled false}}}
   :tooltip     {:shared       true
                 :useHTML      true
                 :headerFormat "<b>{point.key}</b><br/>"
                 :pointFormat  "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: <b>{point.y}</b><br/>"}
   :series      [{:name  "Static train CA"
                  :data  static-train-serie
                  :color "red"}
                 {:name      "Static test CA"
                  :data      static-test-serie
                  :color     "red"
                  :dashStyle "Dash"}
                 {:name   "Train CA"
                  :data   train-serie
                  :color  "blue"
                  :marker {:enabled true :symbol "square"}}
                 {:name      "Test CA"
                  :data      test-serie
                  :color     "blue"
                  :dashStyle "Dash"
                  :marker    {:enabled true :symbol "square"}}]})

(defn- operations-chart-config
  [solvings]
  (let [operations (mapcat
                     (fn [solving]
                       (let [m (:mutation (:best-case solving))
                             o (:operation m)]
                         (if (= :asols.mutations/combined o)
                          (map :operation (:mutations m))
                          [o])))
                     solvings)
        series (for [[operation ops] (group-by identity operations)]
                 [(name operation) (count ops)])]
    {:chart       {:plotBackgroundColor nil
                   :plotBorderWidth     nil
                   :plotShadow          false
                   :height 300}
     :title       {:text "Operations"}
     :plotOptions {:pie {:allowPointSelect true
                         :showInLegend     true}}
     :series      [{:type "pie"
                    :name "Operations used"
                    :data series}]}))

(defcomponent cost-chart [[s1 s2 s3 s4]]
  (render [_]
    (om/build widgets/highchart (costs-chart-config s1 s2 s3 s4))))

(defcomponent ca-chart [[s1 s2 s3 s4]]
  (render [_]
    (om/build widgets/highchart (ca-chart-config s1 s2 s3 s4))))

(defcomponent operations-chart [{:keys [solvings]}]
  (render [_]
    (om/build widgets/highchart (operations-chart-config solvings))))

(defn- series->csv
  [series]
  (str/join "\n\n" (for [serie series]
                   (str (mapv first serie) "\n" (mapv second serie)))))

(defcomponent stats-panel [{:keys [progress solvings metrics]}]
  (render [_]
    (let [[train-cost-data test-cost-data train-ca-data test-ca-data] metrics]
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
          (let [train-serie (cons [1 (get train-cost-data 1)]
                                  (solvings->serie solvings #(-> % :best-case :train-cost)))
                test-serie (cons [1 (get test-cost-data 1)]
                                 (solvings->serie solvings #(-> % :best-case :test-cost)))
                static-train-serie (sort-by first train-cost-data)
                static-test-serie (sort-by first test-cost-data)
                series [train-serie test-serie
                        static-train-serie static-test-serie]]
            [:.col-md-12
             (om/build cost-chart series)
             [:.btn.btn-success.btn-block {:on-click #(log (series->csv series))}
              "Export"]])]
         [:.row
          (let [train-serie (cons [1 (get train-ca-data 1)]
                                  (solvings->serie solvings #(-> % :best-case :train-ca)))
                test-serie (cons [1 (get test-ca-data 1)]
                                 (solvings->serie solvings #(-> % :best-case :test-ca)))
                static-train-serie (sort-by first train-ca-data)
                static-test-serie (sort-by first test-ca-data)
                series [train-serie test-serie
                        static-train-serie static-test-serie]]
            [:.col-md-12
             (om/build ca-chart series)
             [:.btn.btn-success.btn-block {:on-click #(log (series->csv series))}
              "Export"]])]
         [:.row
          [:.col-md-12
           (om/build operations-chart {:solvings solvings})]]]]))))