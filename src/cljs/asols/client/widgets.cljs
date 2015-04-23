(ns asols.client.widgets
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [sablono.core :refer-macros [html]]
            [goog.string :as gstring]
            [goog.string.format]
            [asols.client.utils :refer [debug]]))

(defn checkbox
  "Simple form checkbox field which binds its value to path in cursor"
  [cursor path label-text]
  {:pre [(om/cursor? cursor)
         (coll? path)
         (string? label-text)]}
  [:label.checkbox
   [:input.custom-checkbox
    {:type      "checkbox"
     :checked   (when (get-in cursor path) "checked")
     :on-change #(om/update! cursor path (.. % -target -checked))}]
   [:span.icons
    [:span.icon-checked]
    [:span.icon-unchecked]]
   label-text])

(defcomponent radio
  "Radio choice field which binds selected value to path in cursor
  Calls clean-fn before updating cursor if given"
  [{:keys [cursor path choices clean-fn]
    :or {clean-fn identity}}
   owner]
  (init-state [_] {:inputs nil})

  (did-mount [_]
    (let [node (om/get-node owner)
          inputs (.find (js/jQuery node) "input[data-toggle=radio]")]
      (.radiocheck inputs)
      (om/set-state! owner :inputs inputs)))

  (will-unmount [_]
    (let [inputs (om/get-state owner :inputs)]
      (.radiocheck inputs "destroy")))

  (render [_]
    (html
      (let [input-name (apply str (map name path))
            selected-choice (get-in cursor path)]
        [:.form-group
         [:.col-sm-12
          (for [[title choice] choices]
            [:label.radio
             [:input {:name        input-name
                      :type        "radio"
                      :data-toggle "radio"
                      :value       (str choice)
                      :checked     (= choice selected-choice)
                      :on-change   #(let [value (.. % -target -value)]
                                     (om/update! cursor path (clean-fn value)))}]
             title])]]))))

(defn input
  "Simple input form control which binds its value to path in cursor.
  Calls clean-fn before updating cursor if given."
  ([cursor path]
   (input cursor path identity))
  ([cursor path clean-fn]
   [:input.form-control
    {:value     (get-in cursor path)
     :on-change #(let [value (.. % -target -value)]
                  (om/update! cursor path (clean-fn value)))}]))

(defn progress-bar
  [value]
  (let [percents (-> (* value 100) (int) (min 100) (max 0))]
    [:.progress
     [:.progress-bar {:role "progressbar"
                      :aria-value-now percents
                      :aria-value-min 0
                      :aria-value-max 100
                      :style {:width (str percents "%")}}
      (gstring/format "%d%% complete" percents)]]))

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