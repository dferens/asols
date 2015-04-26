(ns asols.db
  (:require [taoensso.timbre :as timbre]
            [clojure.java.io :as io])
  (:import (java.io File)))

(timbre/refer-timbre)


(def ^:private db-file-name "db.edn")

(defonce db-state (atom {}))

(defn- collection [] (vector))

(defn- remove-document [coll doc-index]
  (vec
    (concat
      (take doc-index coll)
      (drop (inc doc-index) coll))))

;;
;; Api
;;

(defn load!
  []
  (debug "Loading db...")
  (let [^File db-file (io/as-file db-file-name)]
    (when-not (.exists db-file)
      (spit db-file-name {}))
    (->> (slurp db-file-name)
         (read-string)
         (reset! db-state))
    (debug (format "Loaded %d collections, %d documents total"
                   (count @db-state) (reduce + (map count (vals @db-state)))))))

(defn flush!
  []
  (debug "Flushing db...")
  (->> @db-state
       (spit db-file-name)))

(defn create-collection! [name]
  (debug (str "Creating collection " name))
  (when-not (contains? @db-state name)
    (swap! db-state assoc name (collection))))

(defn drop-collection! [name]
  (swap! db-state dissoc name))

(defn insert! [coll-name obj]
  (swap! db-state update-in [coll-name] conj obj))

(defn delete! [coll-name index]
  (swap! db-state update-in [coll-name] #(remove-document % index)))

(defn get-all [coll-name]
  (get @db-state coll-name))