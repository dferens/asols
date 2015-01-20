(ns asols.report
  (:require [net.cgrand.enlive-html :as html :refer [deftemplate]]))

(deftemplate report-template "templates/report.html"
  []
  [:title] (html/content "Report title"))


(defn create [f]
  "Render and save html report f is any argument acceptable to
  (clojure.java.io/output-stream)."
  (->> (report-template)
       (apply str)
       (spit f)))