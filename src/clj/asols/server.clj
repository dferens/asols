(ns asols.server
  (:gen-class :main true)
  (:require [chord.http-kit :refer [with-channel]]
            [clojure.core.async :refer [<! >! close! go go-loop]]
            [clojure.java.browse :refer [browse-url]]
            [compojure.core :refer [defroutes GET]]
            [compojure.handler :refer [site]]
            [org.httpkit.server :as http]
            [ring.util.response :as resp]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [asols.solver :as solver]
            [asols.worker :as worker]
            [asols.graphics :as graphics]))

(defn index [req]
  (-> (slurp "resources/public/templates/index.html")
      (resp/response)
      (resp/header "Content-Type" "text/html; charset=utf-8")))

(defn ws-handler [req]
  (with-channel req chan {:format :transit-json}
    (prn "Client connected")
    (go-loop [{:keys [message]} (<! chan)]
      (prn "Received:" message)
      (let [{:keys [train-opts]} message
            dataset [[[0 0] [0]] [[1 1] [0]] [[1 0] [1]] [[0 1] [1]]]
            start-net (solver/create-start-net 2 1)]
        (loop [net start-net
               current-error nil]
          (let [{:keys [mutation mean-error] :as solving} (solver/step-net net dataset 3 train-opts)]
            (if (or (nil? current-error)
                    (< mean-error current-error))
              (do
                (>! chan (worker/step-command solving))
                (prn "Sent step")
                (recur (:network mutation) mean-error))
              (do
                (>! chan (worker/finished-command))
                (prn "Finished"))))))
      (recur (<! chan)))))

(defroutes app-routes
  (GET "/" [] index)
  (GET "/ws" [] ws-handler))

(defn run [& {:keys [ip port]
              :or {ip "localhost"
                   port 8080}}]
  (-> #'app-routes
      (wrap-reload)
      (wrap-resource "public")
      (wrap-content-type)
      (wrap-not-modified)
      (http/run-server {:ip ip :port port})))

(defn -main
  [& args]
  (let [ip "localhost"
        port 8000]
    (run :ip ip :port port)
    (prn "Server started")
    (browse-url (str "http://" ip ":" port))))
