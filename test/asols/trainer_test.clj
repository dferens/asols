(ns asols.trainer-test
  (:require [clojure.core.matrix :as m :refer [array]]
            [clojure.test :refer :all]
            [taoensso.timbre.profiling :refer [profile p]]
            [asols.network :as net]
            [asols.commands :refer [->TrainOpts ->MutationOpts]]
            [asols.data :as data]
            [asols.trainer :refer :all]
            [asols.solver :as s]))

(deftest backward-pass-test
  (let [net (-> (net/network 2 2 :asols.trainer/sigmoid)
                (net/insert-layer 1 :asols.trainer/sigmoid 1))
        target-vec (array [0.5 1.0])
        forward-vecs (forward-pass net (array [0.5 -1.0]))
        deltas-vecs (backward-pass net target-vec forward-vecs)]
    (is (= (array [0.5 -1.0])
           (first forward-vecs)))
    (is (= [[1] [2]]
           (mapv m/shape deltas-vecs)))))

(deftest backprop-step-delta-test
  (let [net (-> (net/network 2 2 :asols.trainer/sigmoid)
                (net/insert-layer 1 :asols.trainer/sigmoid 2))
        entry {:input-vec (array [0.5 1])
               :target-vec (array [1 1])}
        t-opts (->TrainOpts 0.1 0.5 0.1 100)
        initial-delta-w (get-initial-delta-w net)
        results (backprop-step-delta net entry 1 t-opts initial-delta-w)
        [delta-weights delta-biases] results]
    (is (= [[2 2] [2 2]]
           (mapv m/shape delta-weights)))
    (is (= [[2] [2]]
           (mapv m/shape delta-biases)))))

(deftest train-epoch-test
  (let [net (-> (net/network 2 2 :asols.trainer/sigmoid)
                (net/insert-layer 1 :asols.trainer/sigmoid 2))
        entries [{:input-vec  (array [0 0])
                  :target-vec (array [1 0])}
                 {:input-vec  (array [0 1])
                  :target-vec (array [0 1])}
                 {:input-vec  (array [1 0])
                  :target-vec (array [0 1])}
                 {:input-vec  (array [1 1])
                  :target-vec (array [1 0])}]
        untrained-cost (calc-cost net entries)
        t-opts (->TrainOpts 0.1 0.3 0.0 100)
        trained-net (train-epoch net entries t-opts)
        trained-cost (calc-cost trained-net entries)
        net-shapes (for [layer (:layers net)]
                     [(m/shape (:edges-matrix layer))
                      (m/shape (:biases layer))])
        trained-net-shapes (for [layer (:layers trained-net)]
                             [(m/shape (:edges-matrix layer))
                              (m/shape (:biases layer))])]
    (is (not= net trained-net))
    (is (< trained-cost untrained-cost))
    (is (= net-shapes trained-net-shapes))))

(deftest train-test
  (let [net (-> (net/network 2 2 :asols.trainer/sigmoid)
                (net/insert-layer 1 :asols.trainer/sigmoid 2))
        entries [{:input-vec  (array [0 0])
                  :target-vec (array [1 0])}
                 {:input-vec  (array [0 1])
                  :target-vec (array [0 1])}
                 {:input-vec  (array [1 0])
                  :target-vec (array [0 1])}
                 {:input-vec  (array [1 1])
                  :target-vec (array [1 0])}]
        untrained-cost (calc-cost net entries)
        t-opts (->TrainOpts 0.1 0.5 0.1 100)
        trained-net (train net entries t-opts)
        trained-cost (calc-cost trained-net entries)]
    (is (not= net trained-net))
    (is (< trained-cost untrained-cost))))

(deftest activate-test
  (let [net (-> (net/network 2 2 :asols.trainer/linear)
                (net/set-weight 0 0 0 1.0)
                (net/set-weight 0 0 1 0.0)
                (net/set-weight 0 1 0 0.0)
                (net/set-weight 0 1 1 1.0))]
    (is (= (array [0 0])
           (activate net (array [0 0]))))
    (is (= (array [1 0])
           (activate net (array [1 0]))))))

(deftest calc-squares-error-test
  (let [net (-> (net/network 2 2 :asols.trainer/linear)
                (net/set-weight 0 0 0 1.0)
                (net/set-weight 0 0 1 0.0)
                (net/set-weight 0 1 0 0.0)
                (net/set-weight 0 1 1 1.0))]
    ; ((0.5 - 0)^2 + (1 - 0)^2) / 2 = 1.25 / 2
    ; ((0.5 - 1)^2 + (0 - 0.5)^2) / 2 = 0.5 / 2
    (is (= (/ (+ 1.25 0.5) 2)
           (calc-squares-error net [(data/entry [0 0] [0.5 1])
                                    (data/entry [0.5 0] [1 0.5])])))))

(deftest calc-ca-test
  (let [net (-> (net/network 2 2 :asols.trainer/linear)
                (net/set-weight 0 0 0 1.0)
                (net/set-weight 0 0 1 0.0)
                (net/set-weight 0 1 0 0.0)
                (net/set-weight 0 1 1 1.0))]
    (is (= (double 1/3)
           (calc-ca net [(data/entry [0 1] [1 0])
                         (data/entry [0 1] [0 1])
                         (data/entry [0 1] [1 0])])))))



#_(profile :info :test
  (let [t-opts (->TrainOpts 0.01 0.25 5E-8 3)
        hidden :asols.trainer/relu
        m-opts (->MutationOpts :asols.commands/classification
                               ::data/fer2013
                               hidden 800
                               :asols.trainer/softmax
                               true false false)
        solver (s/create-solver t-opts m-opts)
        {train-entries :train test-entries :test} (s/get-dataset solver)
        initial-net (-> (s/create-start-net solver)
                        (net/insert-layer 2 hidden 100)
                        (s/train-with solver))
        mutations (s/get-mutations solver initial-net)]
    (prn "Initial network:")
    (prn "Train: cost "
         (calc-cost initial-net train-entries)
         " ca: "
         (calc-ca initial-net train-entries))
    (doseq [{:keys [network deleted-edge]} mutations]
      (let [train-ca (calc-ca network train-entries)
            train-cost (calc-cost network train-entries)]
        (prn "Deleted edge: "
             "train cost: " train-cost
             " ca: " train-ca)))))