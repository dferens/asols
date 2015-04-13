(ns asols.server
  (:require [chord.http-kit :refer [with-channel]]
            [clojure.core.async :refer [<! >! close! go go-loop]]
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
            [asols.trainer :as trainer]))

(defn index [req]
  (-> (slurp "resources/public/templates/index.html")
      (resp/response)
      (resp/header "Content-Type" "text/html; charset=utf-8")))

(defn ws-handler [req]
  (with-channel req chan {:format :transit-json}
    (prn "Client connected")
    (go-loop [frame (<! chan)]
      (if (nil? frame)
        (prn "Client disconnected")
        (let [{message :message} frame]
          (prn "Received:" message)
          (if (= (:command message) ::worker/start)
            (let [{:keys [train-opts mutation-opts]} message
                  train-opts (worker/map->TrainOpts train-opts)
                  mutation-opts (worker/map->MutationOpts mutation-opts)
                  dataset [[[0 0] [0]] [[1 1] [0]] [[1 0] [1]] [[0 1] [1]]]
                  start-net (solver/create-start-net 2 1)]
              (loop [net start-net
                     current-error nil]
                (let [solving (solver/step-net net dataset 3 train-opts mutation-opts)
                      {:keys [mutation mean-error]} solving]
                  (>! chan (worker/step-command solving))
                  (prn "Sent step")
                  (if (or (nil? current-error)
                          (< mean-error current-error))
                    (recur (:network mutation) mean-error)
                    (>! chan (worker/finished-command)))))))
          (recur (<! chan)))))))

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
