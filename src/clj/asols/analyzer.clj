(ns asols.analyzer
  (:require [taoensso.timbre :as timbre]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [asols.db :as db]
            [asols.trainer :as t]
            [asols.data :as data]
            [asols.commands :as cmd]
            [asols.solver :as s]
            [asols.network :as net]))

(timbre/refer-timbre)

(db/load!)
(db/create-collection! :solvings)

(defn- get-cases
  [cases operation]
  (-> #(= (:operation (:mutation %)) operation)
      (filter cases)))

(defn write-to-file
  [data file-path]
  (with-open [out-file (io/writer file-path)]
    (csv/write-csv out-file data :separator \tab)))

(defn analyze
  [solvings]
  (doall
    (for [s solvings]
      (db/insert! :solvings s)))
  (db/flush!))
