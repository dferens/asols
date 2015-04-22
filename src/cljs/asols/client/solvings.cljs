(ns asols.client.solvings
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [sablono.core :refer-macros [html]]
            [cljs.core.async :refer [<! >! chan close!]]
            [goog.string :as gstring]
            [goog.string.format])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defmulti mutation-view :operation)

(defmethod mutation-view :asols.mutations/identity [_]
  [:span "nothing"])

(defmethod mutation-view :asols.mutations/add-neuron [m]
  [:span "added node "
   [:span.label.label-primary (name (:added-neuron m))]])

(defmethod mutation-view :asols.mutations/add-edge [m]
  (let [[node-from node-to] (:added-edge m)]
    [:span "added edge "
     [:span.label.label-success (gstring/format "%s -> %s" node-from node-to)]]))

(defmethod mutation-view :asols.mutations/del-neuron [m]
  [:span "removed node "
   [:span.label.label-primary (name (:deleted-neuron m))]])

(defmethod mutation-view :asols.mutations/del-edge [m]
  (let [[node-from node-to] (:deleted-edge m)]
    [:span "removed edge "
     [:span.label.label-success (gstring/format "%s -> %s" node-from node-to)]]))

(defmethod mutation-view :asols.mutations/add-layer [m]
  [:span (gstring/format "added hidden layer at %s " (:layer-index m))
   [:span.label.label-info (name (:layer-type m))]])

(defn format-error [error]
  (gstring/format "%.5f" error))

(defn format-time [ms-took]
  (condp > ms-took
    1E3 (str (int ms-took) " ms")
    1E6 (gstring/format "%.2f sec" (/ ms-took 1E3))))

(defcomponent solving-case-block [{:keys [solving-case hover-chan case-id best?]}]
              (render [_]
                      (let [{:keys [train-error test-error]} solving-case]
                        (html
                          [:tr
                           {:class         (when best? "success")
                            :on-mouse-over #(go (>! hover-chan case-id))
                            :on-mouse-out  #(go (>! hover-chan :none))}
                           [:td (mutation-view (:mutation solving-case))]
                           [:td (format-error train-error)]
                           [:td (format-error test-error)]]))))

(defcomponent solving-block [{:keys [number solving visible?]
                              :or {visible? false}} owner]
  (init-state [_]
    {:visible? visible?
     :selected-case-id :none
     :hover-chan (chan)})

  (will-mount [_]
    (let [hover-chan (om/get-state owner :hover-chan)]
      (go
        (loop [selected-case-num (<! hover-chan)]
          (om/set-state! owner :selected-case-id selected-case-num)
          (recur (<! hover-chan))))))

  (render-state [_ {:keys [visible? selected-case-id hover-chan]}]
    (html
      (let [{:keys [cases best-case ms-took]} solving
            preview-case (if (= selected-case-id :none)
                           best-case
                           (nth cases selected-case-id))]
        [:li.list-group-item.solving
         [:.row {:on-click #(om/update-state! owner :visible? not)}
          [:.col-xs-7
           [:span
            (when number (gstring/format "%d. " number))
            (mutation-view (:mutation best-case))]]
          [:.col-xs-5.stats
           [:span.label.label-danger
            "Test " (format-error (:test-error best-case))]
           [:span.label.label-warning
            "Train " (format-error (:train-error best-case))]
           [:span.label.label-default (format-time ms-took)]]]
         [:.row {:class (when-not visible? "hidden")}
          [:.col-xs-5
           {:dangerouslySetInnerHTML {:__html (:graph preview-case)}}]
          [:.col-xs-7
           [:table.table.table-condensed.table-hover
            [:thead [:tr [:th "Operation"] [:th "Train error"] [:th "Test error"]]]
            [:tbody
             (om/build solving-case-block {:solving-case best-case
                                           :hover-chan hover-chan
                                           :case-id :none
                                           :best? true})
             (for [i (range (count cases))
                   :let [case (nth cases i)]]
               (om/build solving-case-block {:solving-case case
                                             :hover-chan hover-chan
                                             :case-id i
                                             :best? false}))]]]]]))))

(defcomponent solvings-panel [{:keys [solvings]}]
  (render [_]
    (html
      [:.solvings
       [:.panel.panel-primary
        [:.panel-heading "Mutations"]
        [:ul.list-group
         (for [i (range (count solvings))]
           (om/build solving-block
                     {:number  (inc i) :solving (nth solvings i)}
                     {:react-key i}))]]])))