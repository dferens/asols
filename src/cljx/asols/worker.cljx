(ns asols.worker
  (:require #+clj [asols.graphics :as graphics]))


(def ^:private commands
  #{::start ::step ::finished})

(defrecord TrainOpts [learning-rate momentum weight-decay iter-count])

(defrecord MutationOpts [repeat-times remove-edges? remove-nodes?])

(defrecord SolvingCase [number mutation mean-error variance graph])

(defrecord Solving [ms-took cases])

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
  {:command ::step :solving solving})

(defn finished-command []
  {:command ::finished})
