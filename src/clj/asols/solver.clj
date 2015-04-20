(ns asols.solver
  (:require [asols.network :as network]
            [asols.trainer :as trainer]
            [asols.mutations :as mutations]
            [asols.worker]
            [asols.graphics :as graphics])
  (:import (asols.worker TrainOpts SolvingCase Solving MutationOpts)))

(defn create-start-net
  [inputs-count outputs-count mutation-opts]
  (let [hidden-type (:hidden-layer-type mutation-opts)
        out-type (:out-layer-type mutation-opts)
        [net h1] (-> (network/network inputs-count outputs-count out-type)
                     (network/add-layer hidden-type)
                     (network/add-node 1))
        in-nodes (:nodes (first (:layers net)))
        out-nodes (:nodes (last (:layers net)))]
    (-> net
        (network/add-edge (first in-nodes) h1)
        (network/add-edge h1 (first out-nodes)))))


(defn- get-mutations
  [net opts]
  (concat
    (mutations/identity-mutations net)
    (mutations/add-edges-mutations net)
    (mutations/add-neurons-mutations net)
    (when (:remove-nodes? opts) (mutations/remove-neurons-mutations net))
    (when (:remove-edges? opts) (mutations/remove-edges-mutations net))
    (mutations/add-layers-mutations net)))

(defn step-net
  [net dataset train-opts {:keys [repeat-times] :as mutation-opts}]
  {:pre [(instance? TrainOpts train-opts)
         (instance? MutationOpts mutation-opts)]}
  (let [started (System/nanoTime)
        train #(trainer/calc-mean-error % dataset repeat-times train-opts)
        mutations (get-mutations net mutation-opts)
        cases (vec (for [number (range (count mutations))
                         :let [{new-net :network :as mut} (nth mutations number)
                               [mean-error variance] (train new-net)
                               graph (graphics/render-network new-net)]]
                     (SolvingCase. number mut mean-error variance graph)))
        ms-took (/ (double (- (System/nanoTime) started)) 1E6)]
    (Solving. ms-took cases)))
