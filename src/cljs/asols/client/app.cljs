(ns asols.client.app
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [sablono.core :refer-macros [html]]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [<! >! chan close!]]
            [figwheel.client :as fw]
            [asols.commands :refer [->TrainOpts ->MutationOpts] :as commands]
            [asols.client.settings :refer [settings-panel]]
            [asols.client.solvings :refer [solvings-panel mutation-view]]
            [asols.client.stats :refer [failed-solving-panel stats-panel]]
            [asols.client.utils :refer [parse-float str->keyword log debug error]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

;; App state management

(defn mutation-opts []
  (->MutationOpts
    ::commands/classification
    nil 1
    nil
    true true false))

(defonce app-state
  (atom {:connection nil
         :running? false
         :progress nil
         :settings {:train-opts (->TrainOpts 0.3 0.9 5E-4 1000)
                    :mutation-opts (mutation-opts)
                    :hidden-choices []
                    :out-choices []}
         :solvings []
         :failed-solving nil}))

(defn- send-cmd
  [app cmd]
  (debug (str "Sending command: " (:command cmd)))
  (go (>! (:connection app) cmd)))

(defn init [app hidden-choices out-choices]
  (-> app
      (assoc-in [:settings :hidden-choices] hidden-choices)
      (assoc-in [:settings :out-choices] out-choices)
      (assoc-in [:settings :mutation-opts :hidden-type] (first hidden-choices))
      (assoc-in [:settings :mutation-opts :out-type] (first out-choices))))

(defn start [app]
  (let [{:keys [train-opts mutation-opts]} (:settings app)
        start-cmd (commands/start train-opts mutation-opts)]
    (send-cmd app start-cmd))
  (assoc app :running? true
             :solvings []
             :failed-solving nil
             :progress nil))

(defn update-progress [app mutation progress-value]
  (if (:running? app)
    (assoc app :progress {:mutation mutation :value progress-value})
    app))

(defn new-solving [app solving]
  (if (:running? app)
    (update-in app [:solvings] conj solving)
    app))

(defn finish [app failed-solving]
  (if (:running? app)
    (assoc app :failed-solving failed-solving :running? false)
    app))

(defn abort [app]
  (send-cmd app (commands/abort))
  (assoc app :running? false :progress nil))

(defcomponent app [{:keys [settings running? progress solvings] :as cursor} owner]
  (init-state [_]
    {:start-chan (chan)
     :abort-chan (chan)})

  (will-mount [_]
    (let [{:keys [start-chan abort-chan]} (om/get-state owner)]
      (go-loop [_ (<! start-chan)]
               (om/transact! cursor start)
               (recur (<! start-chan)))
      (go-loop [_ (<! abort-chan)]
               (om/transact! cursor abort)
               (recur (<! abort-chan)))
      (go-loop [frame (<! (:connection @cursor))]
               (when-not (nil? frame)
                 (let [{m :message} frame]
                   (debug (str "Received command: " (:command m)))
                   (case (:command m)
                     ::commands/init
                     (om/transact! cursor #(init % (:hidden-choices m) (:out-choices m)))

                     ::commands/progress
                     (om/transact! cursor #(update-progress % (:mutation m) (:value m)))

                     ::commands/step
                     (om/transact! cursor #(new-solving % (:solving m)))

                     ::commands/finished
                     (om/transact! cursor #(finish % (:solving m))))
                   (recur (<! (:connection @cursor))))))))
  (render-state [_ {:keys [abort-chan start-chan]}]
    (html
      [:.container-fluid.app
       [:.row-fluid
        [:.col-md-12
         (om/build settings-panel
           {:start-chan start-chan
            :abort-chan abort-chan
            :running? running?
            :settings settings})]]
       [:.row-fluid
        [:.col-md-12
         (om/build solvings-panel {:solvings solvings})]]

       (when (:failed-solving cursor)
         [:.row-fluid
          [:.col-md-12
           (om/build failed-solving-panel (:failed-solving cursor))]])

       [:.row-fluid
        [:.col-md-12
         (om/build stats-panel {:progress progress
                                :solvings solvings
                                :settings settings})]]])))

(defn- launch []
  (debug "Starting app...")
  (om/root app app-state {:target (.-body js/document)}))

(defn- shutdown []
  (debug "Shutdown...")
  (om/detach-root (.-body js/document)))

(go
  (when (nil? (:connection @app-state))
    (let [ws-chan (ws-ch "ws://localhost:8000/ws" {:format :transit-json})
          {connection :ws-channel error :error} (<! ws-chan)]
      (if error
        (do
          (error "Failed to connect: ")
          (error error))
        (do
          (debug "Connected to websocket channel")
          (swap! app-state assoc :connection connection)
          (launch))))
    (fw/start {:websocket-url "ws://localhost:3449/figwheel-ws"
               :on-jsload #(do (shutdown) (launch))})))
