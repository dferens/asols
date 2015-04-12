(ns asols.solver
  (:require [asols.network :as network]
            [asols.trainer :as trainer]
            [asols.mutations :as mutations]))

(defrecord Solving [mutation mean-error variance])

(defn create-start-net
  [inputs-count outputs-count]
  (let [[net h1] (-> (network/network inputs-count outputs-count)
                     (network/add-layer)
                     (network/add-node))
        in-nodes (:input-layer net)
        out-nodes (:output-layer net)]
    (-> net
        (network/add-edge (first in-nodes) h1)
        (network/add-edge h1 (first out-nodes)))))

(defn step-net
  [net dataset times opts]
  (let [train #(trainer/calc-mean-error % dataset times opts)
        errors (into {} (for [m (mutations/get-mutations net)]
                          [m (train (:network m))]))
        [mutation {:keys [mean-error variance]}] (apply min-key #(:mean-error (val %)) errors)]
    (->Solving mutation mean-error variance)))
