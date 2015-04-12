(ns asols.core
  (:gen-class :main true)
  (:require [clojure.java.browse :refer [browse-url]]
            [asols.server :as server]))

(defn -main
  [& args]
  (let [ip "localhost"
        port 8000]
    (server/run :ip ip :port port)
    (prn "Server started")
    (browse-url (str "http://" ip ":" port))))