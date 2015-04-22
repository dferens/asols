(ns asols.client.settings
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [cljs.core.async :refer [<! >! chan close!]]
            [sablono.core :refer-macros [html]]
            [asols.client.widgets :as widgets]
            [asols.client.utils :refer [parse-float str->keyword]])
  (:require-macros [cljs.core.async.macros :refer [go]]))


(defcomponent settings-panel [{:keys [start-chan abort-chan running? settings]}]
  (render [_]
    (let [[label-width field-width] [4 8]
          label-class (str "col-sm-" label-width)
          input-class (str "col-sm-" field-width)]
      (html
        [:.panel.panel-default.settings
         [:.panel-heading "Settings"]
         [:.panel-body
          [:.row
           [:.col-sm-6
            [:form.form-horizontal
             [:.form-group
              [:label.control-label {:class label-class} "Learning rate"]
              [:div {:class input-class}
               (widgets/input settings [:train-opts :learning-rate] parse-float)]]
             [:.form-group
              [:label.control-label {:class label-class} "Momentum"]
              [:div {:class input-class}
               (widgets/input settings [:train-opts :momentum] parse-float)]]
             [:.form-group
              [:label.control-label {:class label-class} "Weight decay"]
              [:div {:class input-class}
               (widgets/input settings [:train-opts :weight-decay] parse-float)]]
             [:.form-group
              [:label.control-label {:class label-class} "Iterations"]
              [:div {:class input-class}
               (widgets/input settings [:train-opts :iter-count] js/parseInt)]]
             [:.form-group
              [:div {:class (str input-class " col-sm-offset-" label-width)}
               (if running?
                 [:button.btn.btn-block.btn-danger
                  {:type "button" :on-click #(go (>! abort-chan true))}
                  "Abort"]
                 [:button.btn.btn-block.btn-primary
                  {:type "button" :on-click #(go (>! start-chan true))}
                  "Start"])]]]]
           [:.col-sm-6
            [:form.form-horizontal
             [:.form-group
              [:label.control-label {:class label-class} "Hidden layer"]
              [:div {:class input-class}
               [:select.form-control
                {:on-change #(->> (.. % -target -value)
                                  (str->keyword)
                                  (om/update! settings [:mutation-opts :hidden-type]))
                 :value     (:hidden-type (:train-opts settings))}
                (for [choice (:hidden-choices settings)]
                  [:option {:value choice} (name choice)])]]]

             [:.form-group
              [:label.control-label {:class label-class} "Output layer"]
              [:div {:class input-class}
               [:select.form-control
                {:value (:out-type (:train-opts settings))
                 :on-change #(->> (.. % -target -value)
                                  (str->keyword)
                                  (om/update! settings [:mutation-opts :out-type]))}
                (for [choice (:out-choices settings)]
                  [:option {:value choice} (name choice)])]]]

             (widgets/checkbox settings [:mutation-opts :remove-edges?] "Remove edges?")
             (widgets/checkbox settings [:mutation-opts :remove-nodes?] "Remove nodes?")
             (widgets/checkbox settings [:mutation-opts :add-layers?] "Add layers?")]]]]]))))