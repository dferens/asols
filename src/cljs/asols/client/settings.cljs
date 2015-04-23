(ns asols.client.settings
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [cljs.core.async :refer [<! >! chan close!]]
            [sablono.core :refer-macros [html]]
            [asols.client.widgets :as widgets]
            [asols.client.utils :refer [parse-float str->keyword]]
            [asols.commands :as commands])
  (:require-macros [cljs.core.async.macros :refer [go]]))


(defcomponent settings-panel [{:keys [start-chan abort-chan running? settings]}]
  (render [_]
    (html
      [:.panel.panel-default.settings
       [:.panel-heading "Settings"]
       [:.panel-body
        [:.row
         [:.col-sm-6
          [:form.form-horizontal
           [:.form-group
            [:label.control-label.col-sm-4 "Learning rate"]
            [:.col-sm-8
             (widgets/input settings [:train-opts :learning-rate] parse-float)]]
           [:.form-group
            [:label.control-label.col-sm-4 "Momentum"]
            [:.col-sm-8
             (widgets/input settings [:train-opts :momentum] parse-float)]]
           [:.form-group
            [:label.control-label.col-sm-4 "L2 lambda"]
            [:.col-sm-8
             (widgets/input settings [:train-opts :l2-lambda] parse-float)]]
           [:.form-group
            [:label.control-label.col-sm-4 "Iterations"]
            [:.col-sm-8
             (widgets/input settings [:train-opts :iter-count] js/parseInt)]]
           [:.form-group
            [:.col-sm-8.col-sm-offset-4
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
            [:.col-sm-8
             [:.row
              [:.col-sm-6
               [:label.control-label "Hidden layer"]]
              [:.col-sm-6
               [:select.form-control
                {:on-change #(->> (.. % -target -value)
                                  (str->keyword)
                                  (om/update! settings [:mutation-opts :hidden-type]))
                 :value     (:hidden-type (:train-opts settings))}
                (for [choice (:hidden-choices settings)]
                  [:option {:value choice} (name choice)])]]]]
            [:.col-sm-4
             (widgets/input settings [:mutation-opts :hidden-count] js/parseInt)]]

           [:.form-group
            [:.col-sm-4
             [:label.control-label "Output layer"]]
            [:.col-sm-8
             [:select.form-control
              {:value (:out-type (:train-opts settings))
               :on-change #(->> (.. % -target -value)
                                (str->keyword)
                                (om/update! settings [:mutation-opts :out-type]))}
              (for [choice (:out-choices settings)]
                [:option {:value choice} (name choice)])]]]

           [:.form-group
            [:.col-xs-6
             (widgets/checkbox settings [:mutation-opts :remove-edges?] "Remove edges?")]
            [:.col-xs-6
             (widgets/checkbox settings [:mutation-opts :remove-nodes?] "Remove nodes?")]]

           [:.form-group
            [:.col-xs-6
             (widgets/checkbox settings [:mutation-opts :add-layers?] "Add layers?")]]

           (om/build widgets/radio
                     {:cursor settings
                      :path [:mutation-opts :mode]
                      :choices [["Classification" ::commands/classification]
                                ["Regression" ::commands/regression]]
                      :clean-fn str->keyword})]]]]])))