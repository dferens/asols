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
                  dataset [[[0 0] [1 0]]
                           [[1 1] [1 0]]
                           [[1 0] [0 1]]
                           [[0 1] [0 1]]]
                  start-net (solver/create-start-net 2 2)]
              (loop [net start-net
                     current-error nil]
                (let [solving (solver/step-net net dataset train-opts mutation-opts)
                      best-case (first (sort-by :mean-error (:cases solving)))
                      {:keys [mean-error mutation]} best-case
                      better? (or (nil? current-error)
                                  (< mean-error current-error))]
                  (if (and better? (> mean-error 1E-4))
                    (do
                      (>! chan (worker/step-command solving))
                      (prn "Sent step")
                      (recur (:network mutation) mean-error))
                    (do
                      (>! chan (worker/step-command solving))
                      (>! chan (worker/finished-command))))))))
          (recur (<! chan)))))))

(defroutes app-routes
  (GET "/" [] index)
  (GET "/ws" [] ws-handler))

(defn run [& {:keys [ip port]
              :or {ip "localhost"
                   port 8080}}]
  (-> #'app-routes
      (wrap-reload ["src" "target/generated/src/"])
      (wrap-resource "public")
      (wrap-content-type)
      (wrap-not-modified)
      (http/run-server {:ip ip :port port})))
