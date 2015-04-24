(ns asols.logs
  (:require [clojure.string :as string]
            [taoensso.timbre :as timbre]))

(timbre/set-level! :debug)

(timbre/set-config!
  [:fmt-output-fn]
  (fn [{:keys [level throwable message timestamp hostname ns]}
       & [{:keys [nofonts?] :as appender-fmt-output-opts}]]
    ;; <timestamp> <LEVEL> [<ns>] - <message> <throwable>
    (format "%s %s [%s] - %s%s"
            timestamp (-> level name string/upper-case) ns (or message "")
            (or (timbre/stacktrace throwable "\n" (when nofonts? {})) ""))))