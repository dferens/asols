(ns asols.commands)


(def ^:private commands
  #{::init ::start ::abort ::progress ::step ::finished})

(defrecord TrainOpts [learning-rate momentum l2-lambda iter-count])

(defrecord MutationOpts [hidden-type out-type remove-edges? remove-nodes? add-layers?])

(defrecord SolvingCase [mutation train-error test-error graph])

(defrecord Solving [best-case cases ms-took])

#+clj
(defmulti deserialize :command)

#+clj
(defmethod deserialize :default [cmd] cmd)

#+clj
(defn init [hidden-types out-types]
  {:command ::init
   :hidden-choices hidden-types
   :out-choices    out-types})

#+clj
(defn progress
  [mutation value]
  {:pre [(<= 0 value 1)]}
  {:command ::progress
   :mutation mutation
   :value (double value)})

#+clj
(defn step
  [solving]
  {:pre [(instance? Solving solving)]}
  {:command ::step :solving solving})

#+clj
(defn finished [failed-solving]
  {:pre [(instance? Solving failed-solving)]}
  {:command ::finished
   :solving failed-solving})

#+cljs
(defn start
  [train-opts mutation-opts]
  {:pre [(instance? TrainOpts train-opts)
         (instance? MutationOpts mutation-opts)]}
  {:command       ::start
   :train-opts    (into {} train-opts)
   :mutation-opts (into {} mutation-opts)})

#+cljs
(defn abort []
  {:command ::abort})

#+clj
(defmethod deserialize ::start
  [{:keys [train-opts mutation-opts] :as cmd}]
  (merge cmd {:train-opts (map->TrainOpts train-opts)
              :mutation-opts (map->MutationOpts mutation-opts)}))

