(ns asols.client.app
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [sablono.core :refer-macros [html]]
            [chord.client :refer [ws-ch]]
            [chord.http :as http]
            [cljs.core.async :refer [<! >! chan close!]]
            [figwheel.client :as fw]
            [asols.commands :as cmd]
            [asols.client.settings :refer [settings-panel]]
            [asols.client.solvings :refer [solvings-panel mutation-view failed-solving-panel]]
            [asols.client.stats :refer [stats-panel]]
            [asols.client.utils :refer [parse-float str->keyword log debug error]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(cljs.reader/register-tag-parser! 'asols.commands.Solving cmd/map->Solving)
(cljs.reader/register-tag-parser! 'asols.commands.SolvingCase cmd/map->SolvingCase)
(cljs.reader/register-tag-parser! 'asols.commands.TrainOpts cmd/map->TrainOpts)
(cljs.reader/register-tag-parser! 'asols.commands.MutationOpts cmd/map->MutationOpts)

;; App state management

(defn mutation-opts []
  (cmd/->MutationOpts
    ::cmd/classification nil
    nil 1
    nil
    true true false))

(defonce app-state
  (atom {:connection nil
         :running? false
         :progress nil
         :settings {:train-opts (cmd/->TrainOpts 0.3 0.5 5E-8 100)
                    :mutation-opts (mutation-opts)
                    :hidden-types []
                    :out-types []
                    :datasets []}
         :solvings []
         :failed-solving nil}))

(defn- send-cmd
  [app cmd]
  (debug (str "Sending command: " (:command cmd)))
  (go (>! (:connection app) cmd)))

(defn- submit-results
  [state]
  (http/post "/analyze" {:body {"solvings" (:solvings state)}}))

(defn init [app hidden-types out-types datasets]
  (-> app
      (assoc-in [:settings :hidden-types] hidden-types)
      (assoc-in [:settings :out-types] out-types)
      (assoc-in [:settings :datasets] datasets)
      (assoc-in [:settings :mutation-opts :hidden-type] (first hidden-types))
      (assoc-in [:settings :mutation-opts :out-type] (first out-types))
      (assoc-in [:settings :mutation-opts :dataset] (first datasets))))

(defn start [app]
  (let [{:keys [train-opts mutation-opts]} (:settings app)
        start-cmd (cmd/start train-opts mutation-opts)]
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
    (do
      (submit-results app)
      (assoc app :failed-solving failed-solving :running? false))
    app))

(defn abort [app]
  (send-cmd app (cmd/abort))
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
                 (let [{error-text :error m :message} frame]
                   (if error-text
                     (error error-text)
                     (do
                       (debug (str "Received command: " (:command m)))
                       (case (:command m)
                         ::cmd/init
                         (om/transact! cursor #(init % (:hidden-types m) (:out-types m) (:datasets m)))

                         ::cmd/progress
                         (om/transact! cursor #(update-progress % (:mutation m) (:value m)))

                         ::cmd/step
                         (om/transact! cursor #(new-solving % (:solving m)))

                         ::cmd/finished
                         (om/transact! cursor #(finish % (:solving m))))))

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
                                :solvings solvings})]]])))

(defn- launch []
  (debug "Starting app...")
  (om/root app app-state {:target (.-body js/document)}))

(defn- shutdown []
  (debug "Shutdown...")
  (om/detach-root (.-body js/document)))

(go
  (when (nil? (:connection @app-state))
    (let [ws-url (str "ws://" (.. js/window -location -host) "/ws")
          ws-chan (ws-ch ws-url {:format :edn})
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
