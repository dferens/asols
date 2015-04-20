(ns asols.solver
  (:require [clojure.core.async :refer [<! >! close! go go-loop]]
            [clojure.core.matrix.stats :refer [mean variance]]
            [asols.network :as network]
            [asols.trainer :as trainer]
            [asols.mutations :as mutations]
            [asols.commands :as commands]
            [asols.graphics :as graphics]
            [asols.data :as data])
  (:import (asols.commands TrainOpts SolvingCase Solving MutationOpts)))

(defn- create-start-net
  [inputs-count outputs-count mutation-opts]
  (let [hidden-type (:hidden-type mutation-opts)
        out-type (:out-type mutation-opts)
        [net h1] (-> (network/network inputs-count outputs-count out-type)
                     (network/add-layer hidden-type)
                     (network/add-node 1))
        in-nodes (:nodes (first (:layers net)))
        out-nodes (:nodes (last (:layers net)))]
    (-> net
        (network/add-edge (first in-nodes) h1)
        (network/add-edge h1 (first out-nodes)))))

(defn- calc-mean-error
  [net dataset mutation-opts train-opts]
  (let [net-errors (for [_ (range (:repeat-times mutation-opts))]
                     (-> (network/reset-weights net)
                         (trainer/train dataset train-opts)
                         (trainer/calc-error dataset)))]
    [(mean net-errors)
     (variance net-errors)]))

(defn- get-mutations
  "Returns collection of all available mutations for given network"
  [net {:keys [remove-edges? remove-nodes?]}]
  (concat
    (mutations/identity-mutations net)
    (mutations/add-edges-mutations net)
    (mutations/add-neurons-mutations net)
    (when remove-nodes? (mutations/remove-neurons-mutations net))
    (when remove-edges? (mutations/remove-edges-mutations net))
    (mutations/add-layers-mutations net)))

(defn- solve-net
  [net dataset train-opts mutation-opts]
  (let [started (System/nanoTime)
        train #(calc-mean-error % dataset mutation-opts train-opts)
        mutations (get-mutations net mutation-opts)
        cases (vec (for [number (range (count mutations))
                         :let [{new-net :network :as mut} (nth mutations number)
                               [mean-error variance] (train new-net)
                               graph (graphics/render-network new-net)]]
                     (SolvingCase. number mut mean-error variance graph)))
        ms-took (/ (double (- (System/nanoTime) started)) 1E6)
        best-case (first (sort-by :mean-error cases))]
    (Solving. cases best-case ms-took)))

(defn init [in-chan out-chan]
  (go (>! out-chan (commands/init (trainer/hidden-types) (trainer/out-types))))
  (go-loop [command (<! in-chan)]
    (when-not (nil? command)
      (prn "Recieved command:" command)
      (case (:command command)
        ::commands/start
        (let [{:keys [train-opts mutation-opts]} command
              dataset data/xor
              start-net (create-start-net 2 2 mutation-opts)
              error-fn #(calc-mean-error % dataset mutation-opts train-opts)
              solve-fn #(solve-net % dataset train-opts mutation-opts)]
          (loop [net start-net
                 [current-error _] (error-fn start-net)]
            (let [solving (solve-fn net)
                  {:keys [mean-error variance mutation]} (:best-case solving)
                  better? (< mean-error current-error)
                  enough? (< mean-error 1E-4)]
              (if (and better? (not enough?))
                (do
                  (>! out-chan (commands/step solving))
                  (prn "Sent step")
                  (recur (:network mutation) [mean-error variance]))
                (do
                  (prn "Finished")
                  (>! out-chan (commands/finished solving)))))))
       :default
       (prn "Unknown command:" command))
      (recur (<! in-chan)))))