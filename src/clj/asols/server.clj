(ns asols.server
  (:require [chord.http-kit :refer [with-channel]]
            [clojure.core.async :refer [<! >! chan close! go go-loop]]
            [compojure.core :refer [defroutes GET]]
            [compojure.handler :refer [site]]
            [org.httpkit.server :as http]
            [ring.util.response :as resp]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [asols.solver :refer [init]]
            [asols.commands :refer [deserialize]]))

(defn index [req]
  (-> (slurp "resources/public/templates/index.html")
      (resp/response)
      (resp/header "Content-Type" "text/html; charset=utf-8")))

(defn ws-handler [req]
  (with-channel
    req chord-chan
    {:format :transit-json
     :write-ch (chan 10)}
    (prn "Client connected")
    (let [in-chan (chan)
          out-chan chord-chan]
      (go-loop [frame (<! chord-chan)]
        (if (nil? frame)
          (do
            (prn "Client disconnected")
            (close! in-chan))
          (let [{message :message} frame]
            (>! in-chan (deserialize message))
            (recur (<! chord-chan)))))

      (init in-chan out-chan))))

(defroutes app-routes
  (GET "/" [] index)
  (GET "/ws" [] ws-handler))

(defn run [& {:keys [ip port]
              :or {ip "localhost"
                   port 8080}}]
  (-> #'app-routes
      (wrap-reload {:dirs ["src" "target/generated/src"]})
      (wrap-resource "public")
      (wrap-content-type)
      (wrap-not-modified)
      (http/run-server {:ip ip :port port})))
