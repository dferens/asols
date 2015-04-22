(ns asols.client.utils)

(defn str->keyword
  [value]
  (keyword (apply str (rest value))))

(defn parse-float
  [string]
  (if (= string "0.")
    string
    (js/parseFloat string)))

(defn log [msg]
  (.log js/console msg))

(defn debug [msg]
  (.debug js/console msg))

(defn error [msg]
  (.error js/console msg))