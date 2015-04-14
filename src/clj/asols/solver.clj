(ns asols.solver
  (:require [asols.network :as network]
            [asols.trainer :as trainer]
            [asols.mutations :as mutations]
            [asols.worker])
  (:import (asols.worker TrainOpts Solving MutationOpts)))

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


(defn- get-mutations
  [net opts]
  (concat
    (mutations/add-edges-mutations net)
    (mutations/add-neurons-mutations net)
    (when (:remove-nodes? opts) (mutations/remove-neurons-mutations net))
    (when (:remove-edges? opts) (mutations/remove-edges-mutations net))
    (mutations/add-layers-mutations net)))

(defn step-net
  [net dataset train-opts {:keys [repeat-times] :as mutation-opts}]
  {:pre [(instance? TrainOpts train-opts)
         (instance? MutationOpts mutation-opts)]}
  (let [train #(trainer/calc-mean-error % dataset repeat-times train-opts)
        mutations-tried (into {} (for [m (get-mutations net mutation-opts)]
                                   [m (train (:network m))]))
        best-case (apply min-key #(:mean-error (val %)) mutations-tried)
        [mutation {:keys [mean-error variance]}] best-case]
    (Solving. mutation mean-error variance mutations-tried)))