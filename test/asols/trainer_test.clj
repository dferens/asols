(ns asols.trainer-test
  (:require [clojure.core.matrix :as m :refer [array]]
            [clojure.test :refer :all]
            [asols.network :as net]
            [asols.commands :refer [->TrainOpts]]
            [asols.data :as data]
            [asols.trainer :refer :all]))

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
        dataset (data/->Dataset entries entries 2 2)
        t-opts (->TrainOpts 0.1 0.5 0.1 100)
        trained-net (train net dataset t-opts)
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