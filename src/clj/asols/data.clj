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
  (let [full-path (io/file "resources" "datasets" file-name)]
    (csv/read-csv (io/reader full-path) :separator \tab)))

(def xor
  (let [entries [(entry [0 0] [1 0])
                 (entry [1 1] [1 0])
                 (entry [1 0] [0 1])
                 (entry [0 1] [0 1])]]
    (->Dataset entries entries 2 2)))

(def monks1
  "
  Input vector:
    1. a1:    1, 2, 3
    2. a2:    1, 2, 3
    3. a3:    1, 2
    4. a4:    1, 2, 3
    5. a5:    1, 2, 3, 4
    6. a6:    1, 2

  Output vector:
    1. class: 0, 1 : (a1 = a2) or (a5 = 1)
  "
  (let [files ["monks-1_learn.tab" "monks-1_test.tab"]
        read-file
        (fn [path]
          (for [line (drop 3 (read-dataset path))]
            (let [parsed-line (map #(Integer/parseInt %) (butlast line))
                  class-label (first parsed-line)
                  out-vec (if (zero? class-label) [1 0] [0 1])
                  in-vec (rest parsed-line)]
              (entry in-vec out-vec))))
        [train test] (map (comp vec read-file) files)]
    (->Dataset train test 6 2)))