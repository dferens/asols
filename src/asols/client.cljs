(ns asols.client
  (:require [om.core :as om]
            [sablono.core :refer-macros [html]]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [<! >!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)


(defn app-state []
  {:base-net nil
   :mutations []})

(defn app
  [data owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (go
        (let [{chan :ws-channel error :error} (<! (ws-ch "ws://localhost:8000/ws"))]
           (if error
             (.error js/console error)
             (do
               (.debug js/console "Client connected")
               (go-loop
                 [{:keys [message error]} (<! chan)]
                 (when message
                   (.log js/console (str "Got message " message)))
                 (when error
                   (.error js/console error))))))))
    om/IRender
    (render [_]
      (let [{:keys [base-net mutations]} data]
        (html
          [:div.app-wrapper
           [:div.base-net
            [:p "Base network"]]
           [:div.mutations]])))))

(om/root app (app-state)
         {:target (.getElementById js/document "app")})
