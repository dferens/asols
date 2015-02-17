(ns asols.server
  (:require [chord.http-kit :refer [with-channel]]
            [clojure.core.async :refer [<! >! close! go]]
            [compojure.core :refer [defroutes GET]]
            [compojure.handler :refer [site]]
            [org.httpkit.server :as http]
            [ring.util.response :as resp]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.resource :refer [wrap-resource]]))

(defn index [req]
  (resp/file-response "templates/index.html" {:root "resources/public"}))

(defn ws-handler [req]
  (with-channel req ws-ch
    (go
      (let [{:keys [message]} (<! ws-ch)]
        (prn "Message received:" message)
        (>! ws-ch "Hello client from server!")
        (close! ws-ch)))))

(defroutes app-routes
  (GET "/" [] index)
  (GET "/ws" [] ws-handler))

(defn run [& {:keys [ip port]
              :or {ip "localhost"
                   port 8080}}]
  (-> #'app-routes
      (wrap-reload)
      (wrap-resource "public")
      (http/run-server {:ip ip :port port})))