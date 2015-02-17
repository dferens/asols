(ns asols.core
  (:gen-class :main true)
  (:require [clojure.java.browse :refer [browse-url]]
            [asols.trainer :as trainer]
            [asols.network :as network]
            [asols.mutations :as mutations]
            [asols.server :as server]))

(def dataset
  [[[0 0] [0]]
   [[1 1] [0]]
   [[1 0] [1]]
   [[0 1] [1]]])

(defn init-network []
  (let [[net h1] (-> (network/network 2 1)
                     (network/add-layer)
                     (network/add-node 0))
        [i1 i2] (:input-layer net)
        [o1 o2 o3] (:output-layer net)]
    (-> net
        (network/add-edge i1 h1)
        (network/add-edge h1 o1))))

(defn calc-average-error
  "Returns average net error"
  [net times]
  (let [get-error #(-> net
                       (network/reset-weights)
                       (trainer/learn dataset 3000 :learning-rate 0.2)
                       (trainer/calc-error dataset))]
    (/ (reduce + (repeatedly times get-error))
       times)))

(defn mutate-network
  [base-net]
  (let [base-error (calc-average-error base-net 5)]
    (loop [mutations []]
      (let [[last-net last-error] (if (empty? mutations)
                                    [base-net base-error]
                                    (let [[mutation error] (last mutations)]
                                      [(:network mutation) error]))
            net-mutations (mutations/get-mutations last-net)
            errors (into {} (for [m net-mutations]
                              [m (calc-average-error (:network m) 5)]))
           [best-mutation, best-error] (apply min-key val errors)]
        (if (>= best-error last-error)
          [mutations base-error]
          (recur (conj mutations [best-mutation best-error])))))))

(defn -main
  [& args]
  (let [ip "localhost"
        port 8000]
    (server/run :ip ip :port port)
    (prn "Server started")
    (browse-url (str "http://" ip ":" port))))