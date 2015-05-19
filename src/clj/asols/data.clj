(ns asols.data
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.string :as str]
            [clojure.core.matrix :as matrix :refer [array]]))

(matrix/set-current-implementation :vectorz)

(defrecord Entry [input-vec target-vec])

(defrecord Dataset [train test inputs-count outputs-count])

(defn entry
  "Creates dataset entry"
  [input-vec target-vec]
  (->Entry (array input-vec) (array target-vec)))

(defn- read-dataset [file-name]
  (let [full-path (io/file "resources" "datasets" file-name)
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

(defn random-split
  "(random-split [0 1 2 3 4 5] 1/5) -> [[0][1 2 3 4 5]"
  [coll proportion]
  (let [index (Math/floor (* proportion (count coll)))]
    (split-at index (shuffle coll))))

(def ^:private twospirals
  (let [entries (for [line (read-dataset "twospirals.tab")
                      :let [[x y class] line]]
                  (entry [(Double/parseDouble x)
                          (Double/parseDouble y)]
                         (class-vec (Integer/parseInt class) 2)))
        [train-entries test-entries] [entries entries]]
    (->Dataset train-entries test-entries 2 2)))

(defn- parse-fer2013-dataset
  "Facial Expression Recognition Challenge

  The data consists of 48x48 pixel grayscale images of faces. The faces have
  been automatically registered so that the face is more or less centered and
  occupies about the same amount of space in each image.

  Input vector:
    48 * 48 vector of [0 255] doubles

  Output vector:
    1. emotion, one of:
      0 - Angry
      1 - Disgust
      2 - Fear
      3 - Happy
      4 - Sad
      5 - Surprise
      6 - Neutral
  "
  []
  (let [file-path (io/file "resources" "datasets" "fer2013.csv")
        reader (io/reader file-path)
        lines (for [[emotion-str pixels set-name] (rest (csv/read-csv reader))]
                [(Integer/parseInt emotion-str)
                 (map #(double
                        (/ (Integer/parseInt %)
                           255))
                      (str/split pixels #" "))
                 (case set-name
                   "Training" :train
                   "PublicTest" :test)])
        line->entry (fn [[emotion pixels _]]
                      (entry (array pixels) (class-vec emotion 7 0)))
        train-entries (->> lines
                           (filter (fn [[_ _ set]] (= set :train)))
                           (take 5000)
                           (map line->entry))
        test-entries (->> lines
                          (filter (fn [[_ _ set]] (= set :test)))
                          (take 100)
                          (map line->entry))]
    (->Dataset (vec train-entries) (vec test-entries) (* 48 48) 7)))

(defn parse-yale-dataset
  [dataset-file]
  (let [entries (for [line (read-dataset dataset-file)
                      :let [pixels (map #(Double/parseDouble %) (butlast line))
                            target-class (Integer/parseInt (last line))]]
                  (entry (array pixels) (class-vec target-class 5 1)))
        [train-entries test-entries] (random-split entries 0.7)]
    (->Dataset train-entries test-entries (* 26 26) 5)))

(def datasets
  {::monks1     monks1
   ::monks2     monks2
   ::monks3     monks3
   ::twospirals twospirals
   ::yale       (parse-yale-dataset "yale.tab")})

