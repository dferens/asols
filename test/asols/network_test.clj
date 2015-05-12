(ns asols.network-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :as m]
            [asols.network :refer :all]))

(deftest add-layer-test
  (let [net3 (-> (network 2 3 :out)
                 (insert-layer 1 :hidden 3))
        net4 (insert-layer net3 1 :hidden 3)]
    (is (= [[2 3] [3 3]]
           (mapv get-layer-shape (:layers net3))))
    (is (= [[2 3] [3 3] [3 3]]
           (mapv get-layer-shape (:layers net4))))))

(deftest add-node-test
  (let [net (-> (network 2 2 :out)
                (insert-layer 1 :hidden 2)
                (set-layer-edges 1 (m/matrix [[1 2]
                                              [3 4]]))
                (set-layer-edges 2 (m/matrix [[5 6]
                                              [7 8]])))]
    (testing "Input layer"
      (let [new-net (add-node net 0)
            w1 (get-weight new-net 0 2 0)
            w2 (get-weight new-net 0 2 1)]
        (is (= {:layers [{:type :hidden
                          :edges-matrix (m/matrix [[1 2] [3 4] [w1 w2]])
                          :edges-mask (m/matrix [[1 1] [1 1] [1 1]])
                          :biases (m/array [0 0])}
                         {:type :out
                          :edges-matrix (m/matrix [[5 6] [7 8]])
                          :edges-mask (m/matrix [[1 1] [1 1]])
                          :biases (m/array [0 0])}]}
               new-net))))
    (testing "Hidden layer"
      (let [new-net (add-node net 1)
            win1  (get-weight new-net 0 0 2)
            win2  (get-weight new-net 0 1 2)
            wout1 (get-weight new-net 1 2 0)
            wout2 (get-weight new-net 1 2 1)]
        (is (= {:layers [{:type :hidden
                          :edges-matrix (m/matrix [[1 2 win1]
                                                   [3 4 win2]])
                          :edges-mask (m/matrix [[1 1 1] [1 1 1]])
                          :biases (m/array [0 0 0])}
                         {:type :out
                          :edges-matrix (m/matrix [[5 6]
                                                   [7 8]
                                                   [wout1 wout2]])
                          :edges-mask (m/matrix [[1 1] [1 1] [1 1]])
                          :biases (m/array [0 0])}]}
               new-net))))
    (testing "Adding to out layer"
      (let [new-net (add-node net 2)
            wout1 (get-weight new-net 1 0 2)
            wout2 (get-weight new-net 1 1 2)]
        (is (= {:layers [{:type :hidden
                          :edges-matrix (m/matrix [[1 2] [3 4]])
                          :edges-mask (m/matrix [[1 1] [1 1]])
                          :biases (m/array [0 0])}
                         {:type :out
                          :edges-matrix (m/matrix [[5 6 wout1]
                                                   [7 8 wout2]])
                          :edges-mask (m/matrix [[1 1 1] [1 1 1]])
                          :biases (m/array [0 0 0])}]}
               new-net))))))

(deftest del-node-test
  (let [net (-> (network 2 2 :out)
                (insert-layer 1 :hidden 2)
                (set-layer-edges 1 (m/matrix [[1 2]
                                              [3 4]]))
                (set-layer-edges 2 (m/matrix [[5 6]
                                              [7 8]])))]
    (testing "Deleting input layer node"
      (is (= {:layers [{:type :hidden
                        :edges-matrix (m/matrix [[3 4]])
                        :edges-mask (m/matrix [[1 1]])
                        :biases (m/array [0 0])}
                       {:type :out
                        :edges-matrix (m/matrix [[5 6] [7 8]])
                        :edges-mask (m/matrix [[1 1] [1 1]])
                        :biases (m/array [0 0])}]}
             (del-node net 0 0)))
      (is (= {:layers [{:type :hidden
                        :edges-matrix (m/matrix [[1 2]])
                        :edges-mask (m/matrix [[1 1]])
                        :biases (m/array [0 0])}
                       {:type :out
                        :edges-matrix (m/matrix [[5 6] [7 8]])
                        :edges-mask (m/matrix [[1 1] [1 1]])
                        :biases (m/array [0 0])}]}
             (del-node net 0 1))))
    (testing "Deleting hidden layer node"
      (is (= {:layers [{:type :hidden
                        :edges-matrix (m/matrix [[2] [4]])
                        :edges-mask (m/matrix [[1] [1]])
                        :biases (m/array [0])}
                       {:type :out
                        :edges-matrix (m/matrix [[7 8]])
                        :edges-mask (m/matrix [[1 1]])
                        :biases (m/array [0 0])}]}
             (del-node net 1 0)))
      (is (= {:layers [{:type :hidden
                        :edges-matrix (m/matrix [[1] [3]])
                        :edges-mask (m/matrix [[1] [1]])
                        :biases (m/array [0])}
                       {:type :out
                        :edges-matrix (m/matrix [[5 6]])
                        :edges-mask (m/matrix [[1 1]])
                        :biases (m/array [0 0])}]}
             (del-node net 1 1))))
    (testing "Deleting out layer node"
      (is (= {:layers [{:type :hidden
                        :edges-matrix (m/matrix [[1 2] [3 4]])
                        :edges-mask (m/matrix [[1 1] [1 1]])
                        :biases (m/array [0 0])}
                       {:type :out
                        :edges-matrix (m/matrix [[6] [8]])
                        :edges-mask (m/matrix [[1] [1]])
                        :biases (m/array [0])}]}
             (del-node net 2 0)))
      (is (= {:layers [{:type :hidden
                        :edges-matrix (m/matrix [[1 2] [3 4]])
                        :edges-mask (m/matrix [[1 1] [1 1]])
                        :biases (m/array [0 0])}
                       {:type :out
                        :edges-matrix (m/matrix [[5] [7]])
                        :edges-mask (m/matrix [[1] [1]])
                        :biases (m/array [0])}]}
             (del-node net 2 1))))))

(deftest network-test
  (let [net (network 2 2 :test)
        matrix (get-in net [:layers 0 :edges-matrix])]
    (is (= net {:layers [{:type :test
                          :edges-matrix matrix
                          :edges-mask (m/matrix [[1.0 1.0] [1.0 1.0]])
                          :biases (m/array [0 0])}]}))))