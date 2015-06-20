(ns asols.data
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.core.matrix :as matrix :refer [array]]))

(matrix/set-current-implementation :vectorz)

(defrecord Entry [input-vec target-vec])

(defrecord Dataset [train test inputs-count outputs-count])

(defn entry
  "Creates dataset entry"
  [input-vec target-vec]
  (->Entry (array input-vec) (array target-vec)))

(defn- read-dataset [file-name]
  (let [full-path (io/resource (str "datasets/" file-name))
        reader (io/reader full-path)]
    (->> (csv/read-csv reader :separator \tab)
         (drop 3))))

(defn- class-vec
  ([class-num classes-count]
    (class-vec class-num classes-count 1))
  ([class-num classes-count start]
   (for [i (range start (+ classes-count start))]
     (if (= i class-num)
       1.0
       0.0))))

(defn- parse-monks-dataset
  "
  All monks datasets has next attributes:

  Input vector:
    1. a1:    1, 2, 3
    2. a2:    1, 2, 3
    3. a3:    1, 2
    4. a4:    1, 2, 3
    5. a5:    1, 2, 3, 4
    6. a6:    1, 2

  Output vector:
    1. class: 0, 1
  "
  [train-file test-file]
  (let [read-file
        (fn [path]
          (for [line (read-dataset path)]
            (let [parsed-line (map #(Integer/parseInt %) (butlast line))
                  class-label (first parsed-line)
                  out-vec (if (zero? class-label) [1 0] [0 1])
                  [a1 a2 a3 a4 a5 a6] (rest parsed-line)
                  in-vec (vec (concat (class-vec a1 3)
                                      (class-vec a2 3)
                                      (class-vec a3 2)
                                      (class-vec a4 3)
                                      (class-vec a5 4)
                                      (class-vec a6 2)))]
              (entry in-vec out-vec))))
        [train test] (map (comp vec read-file) [train-file test-file])]
    (->Dataset train test 17 2)))

(def ^:private monks1
  "Problem:
    (a1 = a2) or (a5 = 1)"
  (parse-monks-dataset "monks-1_learn.tab" "monks-1_test.tab"))

(def ^:private monks2
  "Problem:
    EXACTLY TWO of {a1 = 1, a2 = 1, a3 = 1, a4 = 1, a5 = 1, a6 = 1}"
  (parse-monks-dataset "monks-2_learn.tab" "monks-2_test.tab"))

(def ^:private monks3
  "Problem:
    (a5 = 3 and a4 = 1) or (a5 /= 4 and a2 /= 3)
    (5% class noise added to the training set)"
  (parse-monks-dataset "monks-3_learn.tab" "monks-3_test.tab"))

(defn split-proportion
  "(split-proportion [0 1 2 3 4 5] 1/5) -> [[0][1 2 3 4 5]"
  [coll proportion]
  (let [index (Math/floor (* proportion (count coll)))]
    (split-at index coll)))

(def ^:private twospirals
  (let [entries (for [line (read-dataset "twospirals.tab")
                      :let [[x y class] line]]
                  (entry [(Double/parseDouble x)
                          (Double/parseDouble y)]
                         (class-vec (Integer/parseInt class) 2)))
        [train-entries test-entries] [entries entries]]
    (->Dataset train-entries test-entries 2 2)))

(defn get-yale-entries
  [file]
  (for [line (read-dataset file)
        :let [pixels (map #(Double/parseDouble %) (butlast line))
              target-class (Integer/parseInt (last line))]]
    (entry (array pixels) (class-vec target-class 10 1))))

(defn parse-yale-dataset
  [train-file test-file]
  (->Dataset
    (get-yale-entries train-file)
    (get-yale-entries test-file)
    (* 26 26)
    10))

(def datasets
  {::monks1     monks1
   ::monks2     monks2
   ::monks3     monks3
   ::twospirals twospirals
   ::yale       (parse-yale-dataset "yale-train.tab" "yale-test.tab")})

