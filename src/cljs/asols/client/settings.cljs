(ns asols.client.settings
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [cljs.core.async :refer [<! >! chan close!]]
            [sablono.core :refer-macros [html]]
            [asols.client.widgets :as widgets]
            [asols.client.utils :refer [parse-int parse-float str->keyword]]
            [asols.commands :as commands])
  (:require-macros [cljs.core.async.macros :refer [go]]))


(defcomponent settings-panel [{:keys [start-chan abort-chan running? settings]}]
  (render [_]
    (html
      [:.panel.panel-default.settings
       [:.panel-heading "Settings"]
       [:.panel-body
        [:.row
         [:.col-sm-5
          [:form.form-horizontal
           [:.form-group
            [:label.control-label.col-sm-8 "Learning rate"]
            [:.col-sm-4 (widgets/input settings [:train-opts :learning-rate] parse-float)]]
           [:.form-group
            [:label.control-label.col-sm-8 "Momentum"]
            [:.col-sm-4 (widgets/input settings [:train-opts :momentum] parse-float)]]
           [:.form-group
            [:label.control-label.col-sm-8 "L2 lambda"]
            [:.col-sm-4 (widgets/input settings [:train-opts :l2-lambda] parse-float)]]
           [:.form-group
            [:label.control-label.col-sm-8 "Iterations"]
            [:.col-sm-4 (widgets/input settings [:train-opts :iter-count] parse-int)]]
           [:.form-group
            [:label.control-label.col-sm-8 "Pretrain iter count"]
            [:.col-sm-4 (widgets/input settings [:mutation-opts :initial-iter-count] parse-int)]]
           [:.form-group
            [:label.control-label.col-sm-8 "Input node prob."]
            [:.col-sm-4 (widgets/input settings [:train-opts :in-node-prob] parse-float)]]
           [:.form-group
            [:label.control-label.col-sm-8 "Hidden node prob."]
            [:.col-sm-4 (widgets/input settings [:train-opts :hidden-node-prob] parse-float)]]
           [:.form-group
            [:.col-sm-8.col-sm-offset-4
             (if running?
               [:button.btn.btn-block.btn-danger
                {:type "button" :on-click #(go (>! abort-chan true))}
                "Abort"]
               [:button.btn.btn-block.btn-primary
                {:type "button" :on-click #(go (>! start-chan true))}
                "Start"])]]]]
         [:.col-sm-7
          [:form.form-horizontal
           [:.form-group
            [:.col-sm-4
             [:label.control-label "Dataset"]]
            [:.col-sm-8
             (om/build widgets/select {:cursor settings
                                       :path [:mutation-opts :dataset]
                                       :choices (:datasets settings)
                                       :clean-fn str->keyword})]]
           [:.form-group
            [:.col-sm-8
             [:.row
              [:.col-sm-6
               [:label.control-label "Hidden layer"]]
              [:.col-sm-6
               (om/build widgets/select {:cursor settings
                                         :path [:mutation-opts :hidden-type]
                                         :choices (:hidden-types settings)
                                         :clean-fn str->keyword})]]]
            [:.col-sm-4
             (widgets/input settings [:mutation-opts :hidden-count] parse-int)]]

           [:.form-group
            [:.col-sm-4
             [:label.control-label "Output layer"]]
            [:.col-sm-8
             (om/build widgets/select {:cursor settings :path [:mutation-opts :out-type]
                                       :choices (:out-types settings)
                                       :clean-fn str->keyword})]]

           [:.form-group
            [:.col-sm-6 [:label.control-label "Max combined count"]]
            [:.col-sm-6 (widgets/input settings [:mutation-opts :max-combined-count] parse-int)]]

           [:.form-group
            [:.col-xs-9 [:label.control-label "Amount of train dataset used for training"]]
            [:.col-xs-3
             (widgets/input settings [:mutation-opts :train-frac] parse-float)]]

           [:.form-group
            [:.col-xs-6
             (om/build widgets/checkbox {:cursor settings :path [:mutation-opts :add-nodes?]
                                         :label "Add nodes?"})]
            [:.col-xs-6
             (om/build widgets/checkbox {:cursor settings :path [:mutation-opts :add-edges?]
                                         :label "Add edges?"})]]
           [:.form-group
            [:.col-xs-6
             (om/build widgets/checkbox {:cursor settings :path [:mutation-opts :remove-nodes?]
                                         :label "Drop nodes?"})]
            [:.col-xs-6
             (om/build widgets/checkbox {:cursor settings :path [:mutation-opts :remove-edges?]
                                         :label "Drop edges?"})]]

           [:.form-group
            [:.col-xs-12
             (om/build widgets/checkbox {:cursor settings :path [:mutation-opts :add-layers?]
                                         :label "Add layers?"})]]]]]]])))