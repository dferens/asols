(ns asols.client.widgets
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [sablono.core :refer-macros [html]]
            [goog.string :as gstring]
            [goog.string.format]))

(defn checkbox
  "Simple form checkbox field which binds its value to path in cursor"
  [cursor path label-text]
  {:pre [(om/cursor? cursor)
         (coll? path)
         (string? label-text)]}
  [:.form-group
   [:.col-sm-12
    [:label.checkbox
     [:input.custom-checkbox
      {:type      "checkbox"
       :checked   (when (get-in cursor path) "checked")
       :on-change #(om/update! cursor path (.. % -target -checked))}]
     [:span.icons
      [:span.icon-checked]
      [:span.icon-unchecked]]
     label-text]]])

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