(ns asols.server
  (:require [chord.http-kit :refer [with-channel]]
            [clojure.core.async :refer [<! >! chan close! go go-loop]]
            [clojure.edn :as edn]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.handler :refer [site]]
            [org.httpkit.server :as http]
            [ring.util.response :as resp]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [taoensso.timbre :as timbre]
            [asols.solver :as solver]
            [asols.graphics :refer [render-network]]
            [asols.commands :as cmd]
            [asols.analyzer :refer [analyze]]
            [asols.logs]))

(timbre/refer-timbre)

(def ^:private readers
  {'asols.commands.Solving cmd/map->Solving
   'asols.commands.SolvingCase cmd/map->SolvingCase
   'asols.commands.TrainOpts cmd/map->TrainOpts
   'asols.commands.MutationOpts cmd/map->MutationOpts})

(defn index [req]
  (-> (slurp "resources/public/templates/index.html")
      (resp/response)
      (resp/header "Content-Type" "text/html; charset=utf-8")))

(defn render-network-page [req]
  (let [network-text (slurp (:body req))
        network (edn/read-string {:readers readers} network-text)]
    (if network
      (-> (render-network network)
          (resp/response)
          (resp/header "Content-Type" "image/svg+xml"))
      (-> (resp/response "")
          (resp/status 400)))))

(defn analyze-page [req]
  (let [body (slurp (:body req))
        {solvings "solvings"} (edn/read-string {:readers readers} body)]
    (debug (format "Received %d solvings for analyze" (count solvings)))
    (future (analyze solvings))
    (resp/response "OK")))

(defn ws-handler [req]
  (with-channel
    req chord-chan
    {:format :edn #_:transit-json
     :write-ch (chan 10)
     :read-ch (chan 10)}
    (debug "Client connected")
    (let [in-chan (chan)
          out-chan chord-chan]
      (go-loop [frame (<! chord-chan)]
        (if (nil? frame)
          (do
            (debug "Client disconnected")
            (close! in-chan))
          (let [{message :message} frame]
            (>! in-chan (cmd/deserialize message))
            (recur (<! chord-chan)))))

      (solver/init in-chan out-chan))))

(defroutes app-routes
  (GET "/" [] index)
  (POST "/analyze" [] analyze-page)
  (POST "/render-network" [] render-network-page)
  (GET "/ws" [] ws-handler))

(defn run [& {:keys [ip port]
              :or {ip "localhost"
                   port 8080}}]
  (-> #'app-routes
      (wrap-reload {:dirs ["src" "target/generated/src"]})
      (wrap-resource "public")
      (wrap-content-type)
      (wrap-not-modified)
      (wrap-params)
      (wrap-keyword-params)
      (http/run-server {:ip ip :port port})))
