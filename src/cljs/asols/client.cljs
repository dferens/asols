(ns asols.client
  (:require [om.core :as om]
            [sablono.core :refer-macros [html]]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [<! >! chan]]
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
      (html
        [:.panel.panel-default
         [:.panel-heading "Settings"]
         [:.panel-body
          [:form.form-horizontal

           [:.form-group
            [:label.col-sm-2.control-label {:for :input-lr} "Learning rate"]
            [:.col-sm-3
             [:input.form-control#input-lr
              {:value lr
               :on-change #(om/set-state! owner :lr (.. % -target -value))}]]]

           [:.form-group
            [:label.col-sm-2.control-label {:for :input-momentum} "Momentum"]
            [:.col-sm-3
             [:input.form-control#input-momentum
              {:value momentum
               :on-change #(om/set-state! owner :momentum (.. % -target -value))}]]]

           [:.form-group
            [:label.col-sm-2.control-label {:for :input-iters} "Iterations"]
            [:.col-sm-3
             [:input.form-control#input-iters
              {:value iters
               :on-change #(om/set-state! owner :iters (.. % -target -value))}]]]

           [:.form-group
            [:.col-sm-3.col-sm-offset-2
             [:button.btn.btn-primary
              {:type     "button"
               :disabled (when running? "disabled")
               :on-click #(go (>! start-chan [lr momentum iters]))}
              "Start"]]]]]]))))

(defn mutation-block [[{:keys [mutation mean-error variance] :as solving} graph]]
  (reify
    om/IRender
    (render [_]
      (html
        [:li.list-group-item
         [:p (pr-str mutation)]
         [:p mean-error]
         [:p variance]
         [:div {:dangerouslySetInnerHTML {:__html graph}}]]))))

(defn mutations-panel [{:keys [solvings] :as cursor}]
  (reify
    om/IRender
    (render [_]
      (html
        [:.panel.panel-default
         [:.panel-heading "Mutations"]
         [:ul.list-group
          (for [i (range (count solvings))]
            (om/build mutation-block
                      (nth solvings i)
                      {:react-key i}))]]))))

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
           (om/build mutations-panel {:solvings solvings})]]]))))


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