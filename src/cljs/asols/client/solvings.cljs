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

(defmethod mutation-view :asols.mutations/combined [m]
  (map mutation-view (:mutations m)))

(defmethod mutation-view :asols.mutations/add-node [m]
  [:span.label.label-success (node->str (:added-node m))])

(defmethod mutation-view :asols.mutations/add-edge [m]
  (let [title (edge->str (:added-edge m))]
    [:span.label.label-success title]))

(defmethod mutation-view :asols.mutations/del-node [m]
  [:span.label.label-danger (node->str (:deleted-node m))])

(defmethod mutation-view :asols.mutations/del-edge [m]
  (let [title (edge->str (:deleted-edge m))]
    [:span.label.label-danger title]))

(defmethod mutation-view :asols.mutations/add-layer [m]
  [:span (format "added layer at %s " (:layer-pos m))
   [:span.label.label-success (name (:layer-type m))]])

(defn format-cost
  [cost-val]
  (format "%.4f" cost-val))

(defn format-ca
  [ca-val]
  (format "%.2f" ca-val))

(def default-case-headers ["Mutation" "Train cost" "Test cost"])

(defn- default-case-data
  [case]
  [(mutation-view (:mutation case))
   (format-cost (:train-cost case))
   (format-cost (:test-cost case))])

(defmulti case-headers :mode)
(defmethod case-headers :default [_] default-case-headers)
(defmethod case-headers :asols.commands/classification [_]
  (concat default-case-headers ["Train CA" "Test CA"]))

(defmulti case-data :mode)
(defmethod case-data :default [case] (default-case-data case))
(defmethod case-data :asols.commands/classification
  [{:keys [train-metrics test-metrics] :as case}]
  (let [[train-ca test-ca] (map #(* 100 %) [train-metrics test-metrics])]
    (concat
      (default-case-data case)
      [(format-ca train-ca)
       (format-ca test-ca)])))

(defn format-time [ms-took]
  (condp > ms-took
    1E3 (str (int ms-took) " ms")
    (* 60 1E3) (format "%.2f sec" (/ ms-took 1E3))
    (* 60 60 1E3) (let [min (int (/ ms-took 60E3))
                        sec (int (rem (/ ms-took 1E3) 60))]
                    (format "%d min %d sec" min sec))))

(defcomponent solving-case-block [{:keys [solving-case hover-chan case-id best?]}]
  (render [_]
    (html
      [:tr
       {:class    (when best? "success")
        :on-click #(go (>! hover-chan case-id))}
       (for [val (case-data solving-case)]
         [:td val])])))

(defn render-network
  [network]
  (go
    (let [params {:body network :req-format :edn}
          {:keys [body]} (<! (http/post "/render-network/" params))]
      body)))

(def ^:private cases-per-page 10)

(defn get-page [coll page-size page-num]
  (->> coll
       (drop (* page-size page-num))
       (take page-size)))

(defn has-page? [coll page-size page-num]
  (and (>= page-num 0)
       (< (* page-size (inc page-num))
          (count coll))))

(defcomponent solving-block [{:keys [number solving visible?]
                              :or {visible? false}}
                             owner]
  (init-state [_]
    {:visible? visible?
     :hover-chan (chan)
     :cases-page 0})

  (will-mount [_]
    (let [hover-chan (om/get-state owner :hover-chan)]
      (go (>! hover-chan :none))
      (go-loop [case-num (<! hover-chan)]
               (let [case (if (= case-num :none)
                            (:best-case solving)
                            (nth (:cases solving) case-num))]
                 (recur (<! hover-chan))))))

  (render-state [_ {:keys [visible? hover-chan cases-page]}]
    (html
      (let [{:keys [cases best-case ms-took]} solving
            visible-cases (get-page cases cases-per-page cases-page)
            [mutation train-cost test-cost] (default-case-data best-case)]
        [:li.list-group-item.solving
         [:.row {:on-click #(om/update-state! owner :visible? not)}
          [:.col-xs-12
           [:span
            (format "%d. " number)
            mutation]
           [:span.label.label-danger.pull-right
            "Test " test-cost]
           [:span.label.label-warning.pull-right
            "Train " train-cost]
           [:span.label.label-default.pull-right
            (format-time ms-took)]]]
         [:.row {:class (when-not visible? "hidden")}
          [:.col-xs-12
           [:.row
            [:.col-xs-12
             [:table.table.table-condensed.table-hover
              [:thead
               [:tr (for [h (case-headers best-case)]
                      [:th h])]]
              [:tbody
               (om/build solving-case-block {:solving-case best-case
                                             :hover-chan   hover-chan
                                             :case-id      :none
                                             :best?        true})
               (for [[i case] (map-indexed vector visible-cases)]
                 (om/build solving-case-block {:solving-case case
                                               :hover-chan   hover-chan
                                               :case-id      i
                                               :best?        false}))]]]]
           [:.row
            [:.col-xs-12
             [:ul.pager
              (when (has-page? cases cases-per-page (dec cases-page))
                [:li
                 [:a
                  {:href "#" :on-click #(om/update-state! owner :cases-page dec)}
                  [:i.fui-arrow-left]]])
              (when (has-page? cases cases-per-page (inc cases-page))
                [:li
                 [:a
                  {:href "#" :on-click #(om/update-state! owner :cases-page inc)}
                  [:i.fui-arrow-right]]])]]]]]]))))

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