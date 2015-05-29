(ns asols.client.stats
  (:require [om.core :as om]
            [om.dom :refer [render-to-str]]
            [om-tools.core :refer-macros [defcomponent]]
            [sablono.core :refer-macros [html]]
            [asols.client.solvings :refer [solving-block mutation-view]]
            [asols.client.widgets :as widgets]
            [asols.client.utils :refer [debug format]]))

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
  [solvings train-values test-values]
  (let [train-serie (cons [1 (get train-values 1)]
                          (solvings->serie solvings #(-> % :best-case :train-cost)))
        test-serie (cons [1 (get test-values 1)]
                         (solvings->serie solvings #(-> % :best-case :test-cost)))]
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
                    :data             (->> (sequence train-values)
                                           (sort-by first))
                    :color            "red"
                    :allowPointSelect true}
                   {:name             "Static test cost"
                    :data             (->> (sequence test-values)
                                           (sort-by first))
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
                    :marker    {:enabled true :symbol "square"}}]}))

(defn- ca-chart-config
  [solvings train-values test-values]
  (let [train-serie (cons [1 (get train-values 1)]
                          (solvings->serie solvings #(-> % :best-case :train-ca)))
        test-serie (cons [1 (get test-values 1)]
                         (solvings->serie solvings #(-> % :best-case :test-ca)))]
    {:chart       {:type   "scatter"
                   :height "500"}
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
                    :data  (sort-by first train-values)
                    :color "red"}
                   {:name      "Static test CA"
                    :data      (sort-by first test-values)
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
                    :marker    {:enabled true :symbol "square"}}]}))

(defcomponent cost-chart [{:keys [solvings train-values test-values]}]
  (render [_]
    (om/build widgets/highchart (costs-chart-config solvings train-values test-values))))

(defcomponent ca-chart [{:keys [solvings train-values test-values]}]
  (render [_]
    (om/build widgets/highchart (ca-chart-config solvings train-values test-values))))

(defcomponent stats-panel [{:keys [progress solvings metrics]}]
  (render [_]
    (let [[train-cost-serie test-cost-serie train-ca-serie test-ca-serie] metrics]
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
           (om/build cost-chart {:solvings solvings
                                 :train-values train-cost-serie
                                 :test-values test-cost-serie})]]
         [:.row
          [:.col-md-12
           (om/build ca-chart {:solvings solvings
                               :train-values train-ca-serie
                               :test-values test-ca-serie})]]]]))))