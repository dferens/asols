(ns asols.client
  (:require [om.core :as om]
            [sablono.core :refer-macros [html]]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [<! >! chan]]
            [goog.string :as gstring]
            [goog.string.format]
            [asols.worker :as worker]
            [figwheel.client :as fw])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)


(defonce app-state
  (atom {:running? false
         :solvings []}))

(defn settings-panel [{:keys [start-chan running?]} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:lr 0.2
       :momentum 0.9
       :iters 3000})

    om/IRenderState
    (render-state [_ {:keys [lr momentum iters]}]
      (let [[label-width field-width] [3 3]
            label-class (str "col-sm-" label-width)
            field-class (str "col-sm-" field-width)]
        (html
          [:.panel.panel-default
           [:.panel-heading "Settings"]
           [:.panel-body
            [:form.form-horizontal

             [:.form-group
              [:label.control-label {:class label-class :for :input-lr}
               "Learning rate"]
              [:div {:class field-class}
               [:input.form-control#input-lr
                {:value     lr
                 :on-change #(om/set-state! owner :lr (.. % -target -value))}]]]

             [:.form-group
              [:label.control-label {:class label-class :for :input-momentum}
               "Momentum"]
              [:div {:class field-class}
               [:input.form-control#input-momentum
                {:value     momentum
                 :on-change #(om/set-state! owner :momentum (.. % -target -value))}]]]

             [:.form-group
              [:label.control-label {:class label-class :for :input-iters}
               "Iterations"]
              [:div {:class field-class}
               [:input.form-control#input-iters
                {:value     iters
                 :on-change #(om/set-state! owner :iters (.. % -target -value))}]]]

             [:.form-group
              [:div {:class [field-class (str "col-sm-offset-" label-width)]}
               [:button.btn.btn-primary.btn-block
                {:type     "button"
                 :disabled (when running? "disabled")
                 :on-click #(go (>! start-chan [lr momentum iters]))}
                "Start"]]]]]])))))

(defmulti mutation-view :operation)

(defmethod mutation-view :add-neuron [m]
  [:p "Added node "
   [:span.label.label-primary (name (:added-neuron m))]])

(defmethod mutation-view :add-edge [m]
  (let [[node-from node-to] (:added-edge m)]
    [:p "Added edge "
     [:span.label.label-success (gstring/format "%s -> %s" node-from node-to)]]))

(defmethod mutation-view :del-edge [m]
  (let [[node-from node-to] (:deleted-edge m)]
    [:p "Removed edge "
     [:span.label.label-success (gstring/format "%s -> %s" node-from node-to)]]))

(defn solving-block [{:keys [number solving graph]} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:visible? true})
    om/IRender
    (render [_]
      (html
        (let [{:keys [visible?]} (om/get-state owner)
              {:keys [mutation mean-error variance mutations-tried]} solving
              operation (name (:operation mutation))
              format-title (partial gstring/format "%d. %s")
              format-error (partial gstring/format "%.5f")
              format-variance (partial gstring/format "%.5f")]
          [:li.list-group-item.solving
           [:.row
            [:.col-sm-8
             [:p {:on-click #(om/update-state! owner :visible? not)}
              (format-title number operation)]]
            [:.col-sm-4.stats
             [:span.label.label-info (format-variance variance)]
             [:span.label.label-danger (format-error mean-error)]]]
           [:.row {:class (when-not visible? "hidden")}
            [:.col-sm-4
             {:dangerouslySetInnerHTML {:__html graph}}]
            [:.col-sm-8
             [:table.table.table-condensed
              [:thead [:tr (for [col-name ["Operation" "Error" "Variance"]]
                             [:th col-name])]]
              [:tbody
               (for [[mutation {variance :variance its-error :mean-error}] mutations-tried
                     :let [best? (= its-error mean-error)]]
                 [:tr {:class (when best? "success")}
                  [:td (mutation-view mutation)]
                  [:td (format-error its-error)]
                  [:td (format-variance variance)]])]]]]])))))

(defn solvings-panel [{:keys [solvings] :as cursor}]
  (reify
    om/IRender
    (render [_]
      (html
        [:.solvings
         [:.panel.panel-default
          [:.panel-heading "Mutations"]
          [:ul.list-group
           (for [i (range (count solvings))
                 :let [[solving graph] (nth solvings i)]]
             (om/build solving-block
                       {:number  (inc i)
                        :solving solving
                        :graph   graph}
                       {:react-key i}))]]]))))

(defn app [{:keys [connection running? solvings] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:start-chan (chan)})

    om/IWillMount
    (will-mount [_]
      (go-loop [[lr momentum iters] (<! (om/get-state owner :start-chan))]
               (om/update! cursor :running? true)
               (om/update! cursor :solvings [])
               (>! connection (worker/start-command :learning-rate lr
                                                    :momentum momentum
                                                    :iter-count iters))
               (recur (<! (om/get-state owner :start-chan))))
      (go-loop [{message :message} (<! connection)]
               (.debug js/console (str "Received:" (pr-str message)))
               (case (:command message)
                 ::worker/step (let [{:keys [solving graph]} message]
                                (om/transact! solvings #(conj % [solving graph])))
                 ::worker/finished (om/update! cursor :running? false))
               (recur (<! connection))))

    om/IRenderState
    (render-state [_ {:keys [start-chan]}]
      (html
        [:.container.app
         [:.row-fluid
          [:.col-md-12
           (om/build settings-panel {:start-chan start-chan :running? running?})]]
         [:.row-fluid
          [:.col-md-12
           (om/build solvings-panel {:solvings solvings})]]]))))


(go (let [ws-chan (ws-ch "ws://localhost:8000/ws" {:format :transit-json})
          {connection :ws-channel error :error} (<! ws-chan)]
      (if error
        (do
          (.error js/console "Failed to connect:")
          (.error js/console error))
        (do
          (.log js/console "Connected to websocket channel")
          (swap! app-state assoc :connection connection)
          (om/root app app-state {:target (.-body js/document)})))))

(fw/start {:websocket-url "ws://localhost:3449/figwheel-ws"})