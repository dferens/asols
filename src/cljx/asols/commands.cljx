(ns asols.commands)


(def ^:private commands
  #{::init ::start ::abort ::progress ::step ::finished})

(def modes
  #{::regression ::classification})

(defrecord TrainOpts [learning-rate momentum l2-lambda iter-count])

(defn train-opts
  [& {:keys [learning-rate momentum l2-lambda iter-count]
      :or {learning-rate 0.1
           momentum 0.9
           l2-lambda 0.1
           iter-count 100}}]
  (->TrainOpts learning-rate momentum l2-lambda iter-count))

(defrecord MutationOpts [mode dataset
                         hidden-type hidden-count
                         out-type
                         remove-edges? remove-nodes? add-layers?])

(defn mutation-opts
  [& {:keys [mode dataset hidden-type hidden-count out-type
             remove-edges? remove-nodes? add-layers?]
      :or {hidden-count 1
           remove-edges? true
           remove-nodes? true
           add-layers? false}}]
  (->MutationOpts mode dataset hidden-type hidden-count out-type
                  remove-edges? remove-nodes? add-layers?))

(defrecord SolvingCase [mode net mutation
                        train-cost test-cost
                        train-metrics test-metrics])

(defrecord Solving [initial-net train-opts mutation-opts best-case cases ms-took])

#+clj
(defmulti deserialize :command)

#+clj
(defmethod deserialize :default [cmd] cmd)

#+clj
(defn init [hidden-types out-types datasets]
  {:command      ::init
   :hidden-types hidden-types
   :out-types    out-types
   :datasets     datasets})

#+clj
(defn progress
  [mutation value]
  {:pre [(<= 0 value 1)]}
  {:command  ::progress
   :mutation mutation
   :value    (double value)})

#+clj
(defn- serialize-case [case]
  {:mode          (:mode case)
   :mutation      (:mutation case)
   :train-cost    (:train-cost case)
   :test-cost     (:test-cost case)
   :train-metrics (:train-metrics case)
   :test-metrics  (:test-metrics case)})

#+clj
(defn- serialize-solving
  [{:keys [best-case cases ms-took]}]
  {:best-case (serialize-case best-case)
   :cases (map serialize-case cases)
   :ms-took ms-took})

#+clj
(defn step
  [solving]
  {:pre [(instance? Solving solving)]}
  {:command ::step
   :solving (serialize-solving solving)})

#+clj
(defn finished
  ([]
    {:command ::finished
     :solving nil})
  ([failed-solving]
   {:pre [(instance? Solving failed-solving)]}
   {:command ::finished
    :solving (serialize-solving failed-solving)}))

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
