(ns asols.worker
  (:require #+clj [asols.graphics :as graphics]))


(def ^:private commands
  #{::init ::start ::step ::finished})

(defrecord TrainOpts [learning-rate momentum weight-decay iter-count])

(defrecord MutationOpts [hidden-layer-type
                         out-layer-type
                         repeat-times
                         remove-edges?
                         remove-nodes?])

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
(defn init-command
  [hidden-layers-types out-layers-types]
  {:command ::init
   :opts {:hidden-layer-choices hidden-layers-types
          :out-layer-choices out-layers-types}})

#+clj
(defn step-command
  [solving]
  {:pre [(instance? Solving solving)]}
  {:command ::step :solving solving})

(defn finished-command []
  {:command ::finished})
