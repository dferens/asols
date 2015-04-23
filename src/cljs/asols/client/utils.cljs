(ns asols.client.utils
  (:require [goog.string :as gstring]
            [goog.string.format]))

(defn str->keyword
  [value]
  (keyword (apply str (rest value))))

(defn parse-float
  [string]
  (if (= "." (last string))
    string)
  (let [cleaned (js/parseFloat string)]
    (if (js/isNaN cleaned)
      string
      cleaned)))

(defn parse-int
  [string]
  (let [cleaned (js/parseInt string)]
    (if (js/isNaN cleaned)
      string
      cleaned)))

(defn log [msg]
  (.log js/console msg))

(defn debug [msg]
  (.debug js/console msg))

(defn error [msg]
  (.error js/console msg))

(defn format [template & args]
  (apply gstring/format template args))