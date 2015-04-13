(ns asols.worker
  (:require #+clj [asols.graphics :as graphics]))


(def ^:private commands
  #{::start ::step ::finished})


(defn start-command
  [& {:keys [learning-rate momentum iter-count
             remove-edges? remove-nodes?]
      :or {learning-rate 0.1
           momentum 0.9
           iter-count 1000
           remove-edges? true
           remove-nodes? true}}]
  {:command    ::start
   :train-opts {:learning-rate learning-rate
                :momentum      momentum
                :iter-count    iter-count}
   :mutation-opts {:remove-edges? remove-edges?
                   :remove-nodes? remove-nodes?}})

#+clj
(defn step-command
  [{mutation :mutation :as solving}]
  {:command ::step
   :solving solving
   :graph   (graphics/render-network (:network mutation))})

(defn finished-command []
  {:command ::finished})
