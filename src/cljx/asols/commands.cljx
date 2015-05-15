(ns asols.commands
  (:require #+clj [clojure.core.matrix :as m]
            #+clj [asols.network :as net]))


(def ^:private commands
  #{::init ::start ::abort ::progress ::step ::finished})

(def modes
  #{::regression ::classification})

(defrecord TrainOpts [learning-rate momentum l2-lambda iter-count])

(defrecord MutationOpts [mode dataset
                         hidden-type hidden-count
                         out-type
                         remove-edges? remove-nodes? add-layers?])

(defrecord SolvingCase [mode net mutation cost train-value test-value])

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
(defn- serialize-case
  [solving-case]
  (update-in solving-case [:net] net/serialize))

#+clj
(defn- serialize-solving
  [solving]
  (-> solving
      (update-in [:initial-net] net/serialize)
      (update-in [:best-case] serialize-case)
      (update-in [:cases] #(map serialize-case %))))

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
