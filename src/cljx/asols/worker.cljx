(ns asols.worker
  (:require #+clj [asols.graphics :as graphics]))


(def ^:private commands
  #{::start ::step ::finished})

(defrecord TrainOpts [learning-rate momentum iter-count])

(defrecord MutationOpts [remove-edges? remove-nodes?])

(defrecord Solving [mutation mean-error variance mutations-tried])

(defn start-command
  [train-opts mutation-opts]
  {:pre [(instance? TrainOpts train-opts)
         (instance? MutationOpts mutation-opts)]}
  {:command       ::start
   :train-opts    (into {} train-opts)
   :mutation-opts (into {} mutation-opts)})

#+clj
(defn step-command
  [solving]
  {:pre [(instance? Solving solving)]}
  {:command ::step
   :solving solving
   :graph   (graphics/render-network (:network (:mutation solving)))})

(defn finished-command []
  {:command ::finished})
