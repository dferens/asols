(ns asols.trainer-test
  (:require [clojure.core.matrix :refer [array]]
            [clojure.test :refer :all]
            [asols.network :as net]
            [asols.data :as data]
            [asols.trainer :refer :all]))

(deftest activate-test
  (let [net (-> (net/network 2 2 :asols.trainer/linear)
                (net/add-edge :a :c 1)
                (net/add-edge :b :d 1))]
    (is (= (activate net [0 0])
           (array [0 0])))
    (is (= (activate net [1 0])
           (array [1 0])))))

(deftest calc-squares-error-test
  (let [net (-> (net/network 2 2 :asols.trainer/linear)
                (net/add-edge :a :c 1)
                (net/add-edge :b :d 1))]
    ; ((0.5 - 0)^2 + (1 - 0)^2) / 2 = 1.25 / 2
    ; ((0.5 - 1)^2 + (0 - 0.5)^2) / 2 = 0.5 / 2
    (is (= (calc-squares-error net [(data/entry [0 0] [0.5 1])
                                    (data/entry [0.5 0] [1 0.5])])
           (/ (+ 1.25 0.5) 2)))))

(deftest calc-ca-test
  (let [net (-> (net/network 2 2 :asols.trainer/linear)
                (net/add-edge :a :c 1)
                (net/add-edge :b :d 1))]
    (is (= (calc-ca net [(data/entry [0 1] [1 0])
                         (data/entry [0 1] [0 1])
                         (data/entry [0 1] [1 0])])
           (double 1/3)))))