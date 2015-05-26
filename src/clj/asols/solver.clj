(ns asols.solver
  (:require [clojure.core.async :refer [<! >! >!! chan alts!! go go-loop]]
            [clojure.core.matrix.stats :refer [mean variance]]
            [com.climate.claypoole :as cpool]
            [taoensso.timbre :as timbre]
            [asols.network :as network]
            [asols.trainer :as trainer]
            [asols.mutations :as m]
            [asols.commands :as commands]
            [asols.data :as data]))

(timbre/refer-timbre)

(defmacro time-it
  "Measures execution time of expr, returns [result ms-took]."
  [expr]
  `(let [start# (System/nanoTime)
         ret# ~expr
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     [ret# elapsed#]))

(defprotocol SolverProtocol
  (get-metrics [this net entries]))

(defrecord ClassificationSolver [train-opts mutation-opts]
  SolverProtocol
  (get-metrics [_ net entries]
    (trainer/calc-ca net entries)))

(defrecord RegressionSolver [train-opts mutation-opts]
  SolverProtocol
  (get-metrics [_ _ _] nil))

(defn- converged?
  [_ solving]
  (< (:train-cost (:best-case solving))
     1E-4))

(defn- sort-cases [cases]
  (sort-by :train-cost cases))

(defn create-solver
  [t-opts m-opts]
  (case (:mode m-opts)
    ::commands/regression (->RegressionSolver t-opts m-opts)
    ::commands/classification (->ClassificationSolver t-opts m-opts)))

(defn get-train-validation-entries
  [solver]
  (let [dataset (data/datasets (:dataset (:mutation-opts solver)))
        [train _] (data/split-proportion (:train dataset) 3/4)]
    [train (:train dataset)]))

(defn get-test-entries
  [solver]
  (-> (:dataset (:mutation-opts solver))
      (data/datasets)
      (:test)))

(defn- make-solving
  [solver net best-case other-cases ms-took]
  (commands/->Solving net
                      (:train-opts solver)
                      (:mutation-opts solver)
                      best-case
                      other-cases
                      ms-took))

(defn- get-mutations
  "Returns sequence of all available network mutations."
  [{m-opts :mutation-opts} net]
  (concat
    (m/identity-mutations net)
    (when (:add-edges? m-opts) (m/add-edge-mutations net))
    (when (:add-nodes? m-opts) (m/add-node-mutations net))
    (when (:remove-nodes? m-opts) (m/del-node-mutations net))
    (when (:remove-edges? m-opts) (m/del-edge-mutations net))
    (when (:add-layers? m-opts) (m/add-layers-mutations net))))

(defn solve-mutation
  "Most important fn here.
  Tests given mutation"
  [{:keys [train-opts] :as solver} prev-net mutation]
  (let [[train-e validation-e] (get-train-validation-entries solver)
        entries [validation-e (get-test-entries solver)]
        mutated-net (m/mutate prev-net mutation)
        new-net (trainer/train mutated-net train-e train-opts)
        [train-cost test-cost] (map #(trainer/calc-cost new-net %) entries)
        [train-metrics test-metrics] (map #(get-metrics solver new-net %) entries)]
    (commands/->SolvingCase
      (:mode (:mutation-opts solver))
      new-net mutation
      train-cost test-cost
      train-metrics test-metrics)))

(defn- solve-mutations
  "Solves mutations in parallel using given thread pool.
  Sends solved mutations to progress-chan while working."
  [solver net mutations tpool progress-chan]
  (time-it
    (vec
      (cpool/pmap
        tpool
        (fn [mutation]
          (let [solving-case (solve-mutation solver net mutation)]
            (>!! progress-chan mutation)
            solving-case))
        mutations))))

(defn- make-combined-cases
  [solver net cases]
  (let [select-count 10
        selected-cases (->> cases
                            (sort-cases)
                            (take-while #(not= ::m/identity (:operation (:mutation %))))
                            (take select-count))]
    (for [select-count (range 2 (inc (count selected-cases)))
          :let [merge-cases (take select-count selected-cases)
                mutation (m/combined-mutation (map :mutation merge-cases))]]
      (solve-mutation solver net mutation))))

(defn- make-progress-chan
  "Returns chan which accepts mutations objects and sends progress commands
  to out-chan."
  [out-chan mutations-count]
  (let [progress-chan (chan 10)]
    (go-loop [done-count 0]
      (let [mutation (<! progress-chan)
            value (/ (inc done-count)
                     mutations-count)]
        (when-not (nil? mutation)
          (>! out-chan (commands/progress mutation value))
          (recur (inc done-count)))))
    progress-chan))

(defn solve-net
  "Mutates given net in all available ways, trains all mutations and returns
  solving record.
  Sends progress commands to out-chan while working.
  Shutdown passed thread pool to stop all running futures."
  [solver net out-chan tpool]
  (try
    (let [mutations (get-mutations solver net)
          progress-chan (make-progress-chan out-chan (count mutations))
          [cases ms-took] (solve-mutations solver net mutations tpool progress-chan)
          all-cases (concat cases (make-combined-cases solver net cases))
          [best-case & other-cases] (sort-cases all-cases)]
      (make-solving solver net best-case other-cases ms-took))
    (catch InterruptedException _
      (debug "Detected thread interrupt"))))

(defn create-start-net
  [{:keys [mutation-opts]}]
  (let [{:keys [hidden-type out-type hidden-count]} mutation-opts
        dataset (data/datasets (:dataset mutation-opts))]
    (-> (network/for-dataset dataset out-type)
        (network/insert-layer 1 hidden-type hidden-count))))

(defn create-initial-case
  [{:keys [train-opts] :as solver}]
  (let [net (create-start-net solver)
        ;trained-net (trainer/train net (:train (get-train-entries solver)) train-opts)
        mutation (first (m/identity-mutations net))]
    (solve-mutation solver net mutation)))

(defn solver-loop
  [solver out-chan abort-chan]
  (cpool/with-shutdown! [tpool (cpool/threadpool (cpool/ncpus))]
    (loop [current-case (create-initial-case solver)]
      (let [current-net (:net current-case)
            solving-chan (go (solve-net solver current-net out-chan tpool))
            [val ch] (alts!! [abort-chan solving-chan])]
        (when (not= ch abort-chan)
          (let [new-solving val
                better? (< (:train-cost (:best-case new-solving))
                           (:train-cost current-case))
                best? (converged? solver new-solving)]
            (if better?
              (do
                (>!! out-chan (commands/step new-solving))
                (debug "Sent step")
                (if best?
                  (when (>!! out-chan (commands/finished))
                    (debug "Finished, found best solution"))
                  (recur (:best-case new-solving))))
              (when (>!! out-chan (commands/finished new-solving))
                (debug "Finished, could not find better solution")))))))))

(defn init [in-chan out-chan]
  (let [abort-chan (chan)
        init-cmd (commands/init (trainer/hidden-types)
                                (trainer/out-types)
                                (keys data/datasets))]
    (go
      (>! out-chan init-cmd)
      (loop [command (<! in-chan)]
        (when-not (nil? command)
          (debug (format "Recieved command: %s" (:command command)))
          (debug (str command))
          (case (:command command)
            ::commands/start (let [{:keys [train-opts mutation-opts]} command
                                   solver (create-solver train-opts mutation-opts)]
                               (go (solver-loop solver out-chan abort-chan)))
            ::commands/abort (>!! abort-chan true)
            :default (warn "Unknown command:" command))
          (recur (<! in-chan)))))))