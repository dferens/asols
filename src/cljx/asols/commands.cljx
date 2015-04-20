(ns asols.commands)


(def ^:private commands
  #{::init ::start ::step ::finished})

(defrecord TrainOpts [learning-rate momentum weight-decay iter-count])

(defrecord MutationOpts [hidden-type out-type repeat-times remove-edges? remove-nodes?])

(defrecord SolvingCase [number mutation mean-error variance graph])

(defrecord Solving [cases best-case ms-took])

#+clj
(defmulti deserialize :command)

#+clj
(defmethod deserialize :default [cmd] cmd)

#+clj
(defn init [hidden-types out-types]
  {:command ::init
   :opts    {:hidden-choices hidden-types
             :out-choices    out-types}})

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

#+clj
(defmethod deserialize ::start
  [{:keys [train-opts mutation-opts] :as cmd}]
  (merge cmd {:train-opts (map->TrainOpts train-opts)
              :mutation-opts (map->MutationOpts mutation-opts)}))

