(ns asols.commands)


(def ^:private commands
  #{::init ::start ::abort ::progress ::step ::finished})

(defrecord TrainOpts [learning-rate momentum l2-lambda iter-count
                      in-node-prob hidden-node-prob])

(defn train-opts
  [& {:keys [learning-rate momentum l2-lambda iter-count
             in-node-prob hidden-node-prob]
      :or {learning-rate 0.1
           momentum 0.9
           l2-lambda 0.1
           iter-count 20
           in-node-prob 1
           hidden-node-prob 1}}]
  (->TrainOpts learning-rate momentum l2-lambda iter-count
               in-node-prob hidden-node-prob))

(defrecord MutationOpts [dataset initial-iter-count
                         hidden-type hidden-count
                         out-type
                         max-combined-count
                         train-frac del-edges-frac
                         add-nodes? add-edges?
                         remove-nodes? remove-edges?
                         add-layers?])

(defn mutation-opts
  [& {:keys [dataset initial-iter-count
             hidden-type hidden-count
             out-type
             max-combined-count
             train-frac del-edges-frac
             add-nodes? add-edges? add-layers?
             remove-edges? remove-nodes?]
      :or {initial-iter-count 300
           hidden-count 1
           max-combined-count 10
           train-frac 1
           del-edges-frac 1
           add-nodes? true
           add-edges? true
           remove-edges? true
           remove-nodes? true
           add-layers? false}}]
  (->MutationOpts dataset initial-iter-count
                  hidden-type hidden-count
                  out-type
                  max-combined-count
                  train-frac del-edges-frac
                  add-nodes? add-edges?
                  remove-edges? remove-nodes?
                  add-layers?))

(defrecord SolvingCase [net mutation
                        train-cost test-cost
                        train-ca test-ca])

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
  {:mutation   (:mutation case)
   :train-cost (:train-cost case)
   :test-cost  (:test-cost case)
   :train-ca   (:train-ca case)
   :test-ca    (:test-ca case)})

#+clj
(defn- serialize-solving
  [{:keys [train-opts best-case cases ms-took]}]
  {:train-opts train-opts
   :best-case (serialize-case best-case)
   :cases (map serialize-case cases)
   :ms-took ms-took})

#+clj
(defn step
  [solving metrics]
  {:pre [(instance? Solving solving)]}
  {:command ::step
   :solving (serialize-solving solving)
   :metrics metrics})

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
