(ns asols.mutations-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :as m]
            [asols.mutations :refer :all]
            [asols.trainer :as t]
            [asols.network :as net]))

(deftest identity-mutations-test
  (let [net (net/network 2 2 ::t/sigmoid)]
    (is (= [{:operation :asols.mutations/identity
             :network   net}]
           (identity-mutations net)))))

(deftest add-node-mutations-test
  (let [net (net/network 2 2 ::t/sigmoid)
        net-with-hidden (net/insert-layer net 1 ::t/sigmoid 2)]
    (is (empty? (add-node-mutations net)))
    (let [mutations (add-node-mutations net-with-hidden)]
      (is (= #{"1[2]"}
             (into #{} (map :added-node mutations))))
      (is (= (seq [[2 3] [3 2]])
             (map net/get-layer-shape (:layers (:network (first mutations)))))))))

(deftest add-edge-mutations-test
  (let [net (net/network 2 2 ::t/sigmoid)
        net-without-edge (net/del-edge net 0 0 1)]
    (is (empty? (add-edge-mutations net)))
    (is (= (seq [["0[0]" "1[1]"]])
           (map :added-edge (add-edge-mutations net-without-edge))))))

(deftest del-node-mutations-test
  (let [net (-> (net/network 2 2 ::t/sigmoid)
                (net/insert-layer 1 ::t/sigmoid 2))]
    (is (= #{{:operation :asols.mutations/del-node
              :network (net/del-node net 1 0)
              :deleted-node "1[0]"}
             {:operation :asols.mutations/del-node
              :network (net/del-node net 1 1)
              :deleted-node "1[1]"}}
           (into #{}  (del-node-mutations net))))))

(deftest del-edge-mutations-test
  (let [net (net/network 2 2 ::t/sigmoid)]
    (is (= #{{:operation :asols.mutations/del-edge
              :network (net/del-edge net 0 0 0)
              :deleted-edge ["0[0]" "1[0]"]}
             {:operation :asols.mutations/del-edge
              :network (net/del-edge net 0 0 1)
              :deleted-edge ["0[0]" "1[1]"]}
             {:operation :asols.mutations/del-edge
              :network (net/del-edge net 0 1 0)
              :deleted-edge ["0[1]" "1[0]"]}
             {:operation :asols.mutations/del-edge
              :network (net/del-edge net 0 1 1)
              :deleted-edge ["0[1]" "1[1]"]}}
           (into #{} (del-edge-mutations net))))))