(ns asols.worker
  (:require #+clj [asols.graphics :as graphics]))


(def ^:private commands
  #{::start ::step ::finished})


(defn start-command
  [& {:keys [learning-rate momentum iter-count]
      :or {learning-rate 0.1
           momentum 0.9
           iter-count 1000}}]
  {:command    ::start
   :train-opts {:learning-rate learning-rate
                :momentum      momentum
                :iter-count    iter-count}})

#+clj
(defn step-command
  [{mutation :mutation :as solving}]
  {:command ::step
   :solving solving
   :graph   (graphics/render-network (:network mutation))})

(defn finished-command []
  {:command ::finished})