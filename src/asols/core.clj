(ns asols.core
  (:gen-class :main true)
  (:require [asols.trainer :as trainer]
            [asols.network :as network]
            [asols.mutations :as mutations]))

(def dataset
  [[[0 0] [0]]
   [[1 1] [0]]
   [[1 0] [1]]
   [[0 1] [1]]])

(defn init-network
  []
  (let [net (-> (network/network 2 1)
                (network/add-layer)
                (network/add-node 0)
                (network/add-node 0)
                (network/add-node 0))
        [i1 i2] (:input-layer net)
        [h1 h2 h3] (seq (first (:hidden-layers net)))
        [o1] (:output-layer net)]
    (-> net
        (network/add-edge i1 h1)
        (network/add-edge i1 h2)
        (network/add-edge i1 h3)
        (network/add-edge i2 h1)
        (network/add-edge i2 h2)
        (network/add-edge i2 h3)

        (network/add-edge h1 o1)
        (network/add-edge h2 o1)
        (network/add-edge h3 o1))))


(defn mutate-network
  "Returns map of network mutations and corresponding test errors"
  [network]
  (reduce
    (fn [errors mutation]
      (let [error (-> (mutations/get-network mutation)
                      (trainer/learn dataset 100 :learning-rate 0.2)
                      (trainer/get-test-error))]
        (assoc errors mutation error)))
    {}
    (mutations/get-mutations network)))


;(loop [network (init-network)]
;  (let [current-error (trainer/get-test-error network)
;        mutated-errors (mutate-network network)
;        best-mutation (min-key val mutated-errors)]
;    (if (< (mutated-errors best-mutation) current-error)
;      (recur (mutations/get-network best-mutation))
;      (network))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [network (init-network)
        learned (trainer/learn network dataset 100)]
    (prn learned)))

