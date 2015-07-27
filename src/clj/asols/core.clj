(ns asols.core
  (:gen-class :main true)
  (:import java.awt.HeadlessException)
  (:require [clojure.java.browse :refer [browse-url]]
            [asols.server :as server]))

(defn -main
  [& args]
  (let [ip (or (first args) "localhost")
        port (Integer/parseInt (or (second args) "8000"))]
    (server/run :ip ip :port port)
    (prn "Server started")
    (try
      (browse-url (str "http://" ip ":" port))
      (catch HeadlessException _))))