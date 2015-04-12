(ns asols.client
  (:require [om.core :as om]
            [sablono.core :refer-macros [html]]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [<! >! chan]]
            [goog.string :as gstring]
            [goog.string.format]
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

(defn solving-block [{:keys [number solving graph]} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:visible? false})
    om/IRender
    (render [_]
      (html
        (let [{:keys [visible?]} (om/get-state owner)
              {:keys [mutation mean-error variance]} solving
              operation (name (:operation mutation))
              title (gstring/format "%d. %s" number operation)
              error-label (gstring/format "%.5f" mean-error)
              variance-label (gstring/format "%.5f" variance)]
          [:li.list-group-item {:on-click #(om/update-state! owner :visible? not)}
           [:.row
            [:.col-sm-8
             [:p title]]
            [:.col-sm-4
             [:span.label.label-info.pull-right variance-label]
             [:span.label.label-danger.pull-right error-label]]]
           [:.row {:class (when-not visible? "hidden")}
            [:div {:dangerouslySetInnerHTML {:__html graph}}]]])))))

(defn solvings-panel [{:keys [solvings] :as cursor}]
  (reify
    om/IRender
    (render [_]
      (html
        [:.panel.panel-default
         [:.panel-heading "Mutations"]
         [:ul.list-group
          (for [i (range (count solvings))
                :let [[solving graph] (nth solvings i)]]
            (om/build solving-block
                      {:number (inc i)
                       :solving solving
                       :graph graph}
                      {:key :number}))]]))))

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
               (>! connection {:command :start
                               :opts    {:learning-rate (double lr)
                                         :momentum      (double momentum)
                                         :iter-count    (int iters)}})
               (recur (<! (om/get-state owner :start-chan))))
      (go-loop [{message :message} (<! connection)]
               (.debug js/console (str "Received:" (pr-str message)))
               (case (:command message)
                 :step (let [{:keys [solving graph]} message]
                         (om/transact! solvings #(conj % [solving graph])))
                 :finished (om/update! cursor :running? false))
               (recur (<! connection))))

    om/IRenderState
    (render-state [_ {:keys [start-chan]}]
      (html
        [:.container
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