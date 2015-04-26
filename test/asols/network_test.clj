(ns asols.network-test
  (:require [clojure.test :refer :all]
            [asols.network :refer :all]))

(deftest next-node-id-test
  (are [x y] (= x y)
             (next-node-id) :a
             (next-node-id :a) :b
             (next-node-id :z) :aa
             (next-node-id :az) :ba
             (next-node-id :zz) :aaa))

(deftest network-protocol-test
  (let [empty-net (network 2 1 :out)]
    (testing "hidden-layers"
      (is (empty? (hidden-layers empty-net)))
      (is (= (hidden-layers (->Network [:a :b :c] nil nil nil))
             [:b])))
    (testing "in-edges"
      (is (empty? (in-edges empty-net :c)))
      (is (= (in-edges (add-edge empty-net :a :c) :c)
             (list [:a :c]))))
    (testing "out-edges"
      (is (empty? (out-edges empty-net :a))
          (= (out-edges (add-edge empty-net :a :c) :a)
             (list [:a :c]))))
    (testing "has-edge?"
      (is (false? (has-edge? empty-net :a :c)))
      (is (true? (has-edge? (add-edge empty-net :a :c) :a :c))))))

(deftest add-layer-test
  (let [net (network 2 1 :out)]
    (is (= (hidden-layers (add-layer net :hidden))
           (hidden-layers (add-layer net :hidden 1))
           (list (hidden-layer :hidden)))))
  (let [net (add-layer (network 2 1 :out) :hidden1)]
    (is (= (hidden-layers (add-layer net :hidden2))
           (list (hidden-layer :hidden2) (hidden-layer :hidden1))))
    (is (= (hidden-layers (add-layer net :hidden2 2))
           (list (hidden-layer :hidden1) (hidden-layer :hidden2))))))

(deftest add-node-test
  (let [base-net (add-layer (network 1 1 :out) :hidden)]
    (let [[net node] (add-node base-net 0)]
      (is (= node :c))
      (is (= (:nodes (first (:layers net))) [:a :c])))
    (let [[net node] (add-node base-net 1)]
      (is (= node :c)
          (= (:nodes (first (hidden-layers net))) [:c])))
    (let [[net node] (add-node base-net 2)]
      (is (= node :c)
          (= (:nodes (last (:layers net))) [:c])))))

(deftest del-node-test
  (let [net (-> (network 2 1 :out) (add-edge :a :c) (add-edge :b :c))
        new-net (del-node net 0 :b)]
    (is (= (:layers new-net)
           [(in-layer [:a]) (out-layer :out [:c])])
        (= (:edges new-net) {}))))

(deftest add-del-edge-test
  (let [net (network 2 1 :out)
        net-with-edge (add-edge net :a :c)]
    (is (has-edge? net-with-edge :a :c))
    (is (= (del-edge net-with-edge [:a :c]) net))))

(deftest move-edge-test
  (let [net (add-edge (network 2 1 :out) :a :c)
        moved-edge-net (move-edge net [:a :c] [:b :c])]
    (is (false? (has-edge? moved-edge-net :a :c)))
    (is (true? (has-edge? moved-edge-net :b :c)))
    (is (= (get-weight net [:a :c]) (get-weight moved-edge-net [:b :c])))))

(deftest split-node-test
  (testing "Splitting node without hidden layers"
    (let [net (-> (network 2 2 :out) ; [a b] [c d]
                  (add-edge :a :c)
                  (add-edge :b :c) (add-edge :b :d)
                  (add-layer :hidden)
                  (split-node 2 :c)    ; becomes e
                  (split-node 2 :d))]  ; becomes f
      (is (= (map :nodes (:layers net))
             (list [:a :b] [:e :f] [:c :d])))
      (is (= (into #{} (keys (:edges net)))
             #{[:a :e] [:b :e] [:b :f]
               [:e :c] [:e :d]
               [:f :c] [:f :d]}))))
  (testing "Splitting node with hidden layer"
    (let [net (-> (network 2 1 :out)  ; [a b] [c]
                  (add-layer :hidden)
                  (add-node 1) (first)  ; becomes d
                  (add-node 1) (first)  ; becomes e
                  (add-edge :a :d)
                  (add-edge :b :d) (add-edge :b :e)
                  (add-edge :d :c) (add-edge :e :c)
                  (add-layer :hidden 1)
                  (split-node 2 :d)   ; becomes f
                  (split-node 2 :e))] ; becomes g
      (is (= (into #{} (keys (:edges net)))
             #{[:a :f] [:b :f] [:b :g]
               [:f :d] [:f :e] [:g :d] [:g :e]
               [:d :c] [:e :c]})))))

(deftest full-connect-test
  (let [net (-> (network 2 1 :out) (add-layer :hidden))
        [new-net new-node] (add-node net 1)
        connected-net (full-connect new-net 1 new-node)]
    (is (= (into #{} (keys (:edges connected-net)))
           #{[:a :d] [:b :d] [:d :c]}))))

(deftest network-test
  (is (= (network 1 2 :out-type)
         (->Network [(->Layer ::asols.network/input [:a])
                     (->Layer :out-type [:b :c])]
                    {}
                    {}
                    :d))))
