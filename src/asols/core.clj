(ns asols.core
  (:require [asols.trainer :as trainer]
            [asols.network :as network]
            [asols.mutations :as mutations]))

(defn init-network
  []
  (-> (network/network 2 1)
      (network/add-layer)))


(defn mutate-network
  "Returns map of network mutations and corresponding test errors"
  [network]
  (reduce
    (fn [errors mutation]
      (->> (mutations/get-network mutation)
           (trainer/learn)
           (trainer/get-test-error)
           (assoc errors mutation)))
    {}
    (mutations/get-mutations network)))

(defn algo
  []
  (loop [network (init-network)]
    (let [current-error (trainer/get-test-error network)
          mutated-errors (mutate-network network)
          best-mutation (min-key val mutated-errors)]
      (if (< (mutated-errors best-mutation) current-error)
        (recur (mutations/get-network best-mutation))
        (network)))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

