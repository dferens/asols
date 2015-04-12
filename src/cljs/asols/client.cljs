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
  (atom {:connection :nil
         :running? false
         :settings {:learning-rate 0.3
                    :momentum 1.0
                    :iter-count 2000
                    :remove-edges? true
                    :remove-nodes? false}
         :solvings []}))

(defn settings-panel [{:keys [start-chan running? settings]} owner]
  (reify
    om/IRender
    (render [_]
      (let [[label-width field-width] [6 6]
            label-class (str "col-sm-" label-width)
            field-class (str "col-sm-" field-width)]
        (html
          [:.panel.panel-default.settings
           [:.panel-heading "Settings"]
           [:.panel-body
            [:.row
             [:.col-sm-6
              [:form.form-horizontal
               [:.form-group
                [:label.control-label {:class label-class :for :input-lr}
                 "Learning rate"]
                [:div {:class field-class}
                 [:input.form-control#input-lr
                  {:value     (:learning-rate settings)
                   :on-change #(om/update! settings :learning-rate (.. % -target -value))}]]]

               [:.form-group
                [:label.control-label {:class label-class :for :input-momentum}
                 "Momentum"]
                [:div {:class field-class}
                 [:input.form-control#input-momentum
                  {:value     (:momentum settings)
                   :on-change #(om/update! settings :momentum (.. % -target -value))}]]]

               [:.form-group
                [:label.control-label {:class label-class :for :input-iters}
                 "Iterations"]
                [:div {:class field-class}
                 [:input.form-control#input-iters
                  {:value     (:iter-count settings)
                   :on-change #(om/update! settings :iter-count (.. % -target -value))}]]]

               [:.form-group
                [:div {:class [field-class (str "col-sm-offset-" label-width)]}
                 [:button.btn.btn-primary.btn-block
                  {:type     "button"
                   :disabled (when running? "disabled")
                   :on-click #(go (>! start-chan {}))}
                  "Start"]]]]]
             [:.col-sm-6
              [:form.form-horizontal
               [:.form-group
                [:.col-sm-12
                 [:label.checkbox
                  [:input.custom-checkbox
                   {:type      "checkbox"
                    :checked   (when (:remove-edges? settings) "checked")
                    :on-change #(om/update! settings :remove-edges? (.. % -target -checked))}]
                  [:span.icons
                   [:span.icon-checked]
                   [:span.icon-unchecked]]
                  "Remove edges?"]]]
               [:.form-group
                [:.col-sm-12
                 [:label.checkbox
                  [:input.custom-checkbox
                   {:type      "checkbox"
                    :checked   (when (:remove-nodes? settings) "checked")
                    :on-change #(om/update! settings :remove-nodes? (.. % -target -checked))}]
                  [:span.icons
                   [:span.icon-checked]
                   [:span.icon-unchecked]]
                  "Remove nodes?"]]]]]]]])))))

(defmulti mutation-view :operation)

(defmethod mutation-view :add-neuron [m]
  [:p "Added node "
   [:span.label.label-primary (name (:added-neuron m))]])

(defmethod mutation-view :add-edge [m]
  (let [[node-from node-to] (:added-edge m)]
    [:p "Added edge "
     [:span.label.label-success (gstring/format "%s -> %s" node-from node-to)]]))

(defmethod mutation-view :del-neuron [m]
  [:p "Removed node "
   [:span.label.label-primary (name (:deleted-neuron m))]])

(defmethod mutation-view :del-edge [m]
  (let [[node-from node-to] (:deleted-edge m)]
    [:p "Removed edge "
     [:span.label.label-success (gstring/format "%s -> %s" node-from node-to)]]))

(defn solving-block [{:keys [number solving graph]} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:visible? false})
    om/IRender
    (render [_]
      (html
        (let [{:keys [visible?]} (om/get-state owner)
              {:keys [mutation mean-error variance mutations-tried]} solving
              format-error (partial gstring/format "%.5f")
              format-variance (partial gstring/format "%.5f")]
          [:li.list-group-item.solving
           [:.row
            [:.col-sm-8
             [:p {:on-click #(om/update-state! owner :visible? not)}
              (mutation-view mutation)]]
            [:.col-sm-4.stats
             [:span.label.label-info (format-variance variance)]
             [:span.label.label-danger (format-error mean-error)]]]
           [:.row {:class (when-not visible? "hidden")}
            [:.col-sm-5
             {:dangerouslySetInnerHTML {:__html graph}}]
            [:.col-sm-7
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

(defn app [{:keys [connection settings running? solvings] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:start-chan (chan)})

    om/IWillMount
    (will-mount [_]
      (go-loop []
               (<! (om/get-state owner :start-chan))
               (om/update! cursor :running? true)
               (om/update! cursor :solvings [])
               (>! connection (worker/start-command :learning-rate (js/parseFloat (:learning-rate settings))
                                                    :momentum (js/parseFloat (:momentum settings))
                                                    :iter-count (js/parseInt (:iter-count settings))
                                                    :remove-edges? (:remove-edges? settings)
                                                    :remove-nodes? (:remove-nodes settings)))
               (recur))
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
           (om/build settings-panel {:start-chan start-chan
                                     :running? running?
                                     :settings settings})]]
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