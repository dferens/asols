(ns asols.client.solvings
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [sablono.core :refer-macros [html]]
            [cljs.core.async :refer [<! >! chan close!]]
            [chord.http :as http]
            [asols.client.utils :refer [format]]
            [asols.client.utils :refer [debug]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn- node->str
  [[layer-i node-i]]
  (format "%d[%d]" layer-i node-i))

(defn- edge->str
  [[node-from node-to]]
  (format "%s -> %s" (node->str node-from) (node->str node-to)))

(defmulti mutation-view :operation)

(defmethod mutation-view :asols.mutations/identity [_]
  [:span "nothing"])

(defmethod mutation-view :asols.mutations/add-node [m]
  [:span "added node "
   [:span.label.label-primary (node->str (:added-node m))]])

(defmethod mutation-view :asols.mutations/add-edge [m]
  (let [title (edge->str (:added-edge m))]
    [:span "added edge "
     [:span.label.label-success title]]))

(defmethod mutation-view :asols.mutations/del-node [m]
  [:span "removed node "
   [:span.label.label-primary (node->str (:deleted-node m))]])

(defmethod mutation-view :asols.mutations/del-edge [m]
  (let [title (edge->str (:deleted-edge m))]
    [:span "removed edge "
     [:span.label.label-success title]]))

(defmethod mutation-view :asols.mutations/add-layer [m]
  [:span (format "added hidden layer at %s " (:layer-pos m))
   [:span.label.label-info (name (:layer-type m))]])

(defmulti format-net-value (fn [solver & _] (:mode solver)))
(defmethod format-net-value :asols.commands/regression
  [_ val]
  (format "%.3f" val))
(defmethod format-net-value :asols.commands/classification
  [_ val]
  (format "%.2f %%" (* 100 (- 1.0 val))))

(defn format-cost
  [solving-case]
  (format "%.4f" (:cost solving-case)))

(defmulti net-value-title :mode)
(defmethod net-value-title :asols.commands/regression [_] "error")
(defmethod net-value-title :asols.commands/classification [_] "CA")

(defn format-time [ms-took]
  (condp > ms-took
    1E3 (str (int ms-took) " ms")
    1E6 (format "%.2f sec" (/ ms-took 1E3))))

(defcomponent solving-case-block [{:keys [solving-case hover-chan case-id best?]}]
  (render [_]
    (html
      [:tr
       {:class         (when best? "success")
        :on-click #_:on-mouse-over #(go (>! hover-chan case-id))
        #_:on-mouse-out  #_#(go (>! hover-chan :none))}
       [:td (mutation-view (:mutation solving-case))]
       [:td (format-cost solving-case)]
       [:td (format-net-value solving-case (:train-value solving-case))]
       [:td (format-net-value solving-case (:test-value solving-case))]
       [:td [:button.btn.btn-success.btn-xs
             {:on-click #(do
                          (let [serialized-net (pr-str (:network solving-case))]
                            (.prompt js/window serialized-net)
                            (debug serialized-net)))}
             "Get network"]]])))

(defn render-network
  [network]
  (go
    (let [params {:body network :req-format :edn}
          {:keys [body]} (<! (http/post "/render-network/" params))]
      body)))

(defcomponent solving-block [{:keys [number solving visible?]
                              :or {visible? false}} owner]
  (init-state [_]
    {:visible? visible?
     :hover-chan (chan)})

  (will-mount [_]
    (let [hover-chan (om/get-state owner :hover-chan)]
      (go (>! hover-chan :none))
      (go-loop [case-num (<! hover-chan)]
               (let [case (if (= case-num :none)
                            (:best-case solving)
                            (nth (:cases solving) case-num))]
                 (recur (<! hover-chan))))))

  (render-state [_ {:keys [visible? hover-chan]}]
    (html
      (let [{:keys [cases best-case ms-took]} solving]
        [:li.list-group-item.solving
         [:.row {:on-click #(om/update-state! owner :visible? not)}
          [:.col-xs-12
           [:span
            (when number (format "%d. " number))
            (mutation-view (:mutation best-case))]
           [:span.label.label-danger.pull-right
            "Test " (format-net-value best-case (:test-value best-case))]
           [:span.label.label-warning.pull-right
            "Train " (format-net-value best-case (:train-value best-case))]
           [:span.label.label-info.pull-right
            "Cost " (format-cost best-case)]
           [:span.label.label-default.pull-right
            (format-time ms-took)]]]
         [:.row {:class (when-not visible? "hidden")}
          [:.col-xs-12
           [:table.table.table-condensed.table-hover
            [:thead
             [:tr
              [:th "Operation"]
              [:th "Cost"]
              [:th (str "Train " (net-value-title best-case))]
              [:th (str "Test " (net-value-title best-case))]]]
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

(defcomponent failed-solving-panel [solving]
  (render [_]
    (html
      [:.panel.panel-info
       [:.panel-heading "Further tryings:"]
       [:ul.list-group
        (om/build solving-block {:solving solving :visible? true})]])))