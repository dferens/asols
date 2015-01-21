(ns asols.report
  (:require [net.cgrand.reload]
            [net.cgrand.enlive-html :as html :refer [deftemplate clone-for content]]
            [asols.graphics :refer [render-network]]))

(net.cgrand.reload/auto-reload *ns*)

(deftemplate report-template "templates/report.html"
  [net error mutations-errors]
  [:title] (content "Report title")
  [:.base-net :.graph] (html/html-content (render-network net))
  [:.base-net :.error] (content (str error))
  [:.mutations :.mutation] (clone-for [[{net :network op :operation} error] mutations-errors]
                                      [:.operation] (content (name op))
                                      [:.error] (content (str error))
                                      [:.graph] (html/html-content (render-network net))))


(defn create [f net error mutations-errors]
  "Render and save html report f is any argument acceptable to
  (clojure.java.io/output-stream)."
  (->> (report-template net error mutations-errors)
       (apply str)
       (spit f)))