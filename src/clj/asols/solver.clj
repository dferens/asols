(ns asols.solver
  (:require [clojure.core.async :refer [<! >! >!! chan alts!! go go-loop]]
            [clojure.core.matrix.stats :refer [mean variance]]
            [com.climate.claypoole :as cpool]
            [taoensso.timbre :as timbre]
            [asols.network :as network]
            [asols.trainer :as trainer]
            [asols.mutations :as mutations]
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
  (converged? [this solving])
  (get-error-val [this net entries]))

(defrecord ClassificationSolver [train-opts mutation-opts]
  SolverProtocol
  (converged? [_ solving]
    (= 0.0 (:train-value (:best-case solving))))
  (get-error-val [_ net entries]
    (- 1.0 (trainer/calc-ca net entries))))

(defrecord RegressionSolver [train-opts mutation-opts]
  SolverProtocol
  (converged? [_ solving]
    (< (:test-value (:best-case solving))
       1E-4))
  (get-error-val [_ net entries]
    (trainer/calc-cost net entries)))

(defn create-solver
  [t-opts m-opts]
  (case (:mode m-opts)
    ::commands/regression (->RegressionSolver t-opts m-opts)
    ::commands/classification (->ClassificationSolver t-opts m-opts)))

(defn get-dataset
  [solver]
  (get data/datasets (:dataset (:mutation-opts solver))))

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
  [{:keys [mutation-opts]} net]
  (let [{:keys [remove-nodes? remove-edges? add-layers?]} mutation-opts]
    (concat
      (mutations/identity-mutations net)
      (mutations/add-edge-mutations net)
      (mutations/add-node-mutations net)
      (when remove-nodes? (mutations/del-node-mutations net))
      (when remove-edges? (mutations/del-edge-mutations net))
      (when add-layers? (mutations/add-layers-mutations net)))))

(defn- make-solving-case
  [solver mutation train-value]
  (let [test-entries (:test (get-dataset solver))
        test-value (get-error-val solver (:network mutation) test-entries)
        solving-mode (:mode (:mutation-opts solver))]
    (commands/->SolvingCase solving-mode mutation 0.0 train-value test-value)))

(defn solve-mutation
  "Most important fn here.
  Tests given mutation"
  [solver mutation]
  (let [train-entries (:train (get-dataset solver))
        t-opts (:train-opts solver)
        trained-net (trainer/train (:network mutation) train-entries t-opts)]
    (make-solving-case
      solver
      (assoc mutation :network trained-net)
      (get-error-val solver trained-net train-entries))))

(defn- solve-mutations
  "Solves mutations in parallel using given thread pool.
  Sends solved mutations to progress-chan while working."
  [solver mutations tpool progress-chan]
  (time-it
    (cpool/pmap
      tpool
      (fn [mutation]
        (let [solving-case (solve-mutation solver mutation)]
          (>!! progress-chan mutation)
          solving-case))
      mutations)))

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
          [cases ms-took] (solve-mutations solver mutations tpool progress-chan)
          sorted-cases (sort-by :train-value cases)
          [best-case & other-cases] sorted-cases]
      (make-solving solver net best-case other-cases ms-took))
    (catch InterruptedException _
      (debug "Detected thread interrupt"))))

(defn create-start-net
  [{:keys [train-opts mutation-opts] :as solver}]
  (let [{:keys [inputs-count outputs-count]} (get-dataset solver)
        {:keys [hidden-type out-type hidden-count]} mutation-opts]
    (-> (network/network inputs-count outputs-count out-type)
        (network/insert-layer 1 hidden-type hidden-count)
        (trainer/train (:train (get-dataset solver)) train-opts))))

(defn create-initial-case
  [solver]
  (let [net (create-start-net solver)
        mutation (first (mutations/identity-mutations net))]
    (solve-mutation solver mutation)))

(defn solver-loop
  [solver out-chan abort-chan]
  (let [entries (:test (get-dataset solver))]
    (cpool/with-shutdown! [tpool (cpool/threadpool (cpool/ncpus))]
      (loop [current-case (create-initial-case solver)]
        (let [current-net (:network (:mutation current-case))
              solving-chan (go (solve-net solver current-net out-chan tpool))
              [val ch] (alts!! [abort-chan solving-chan])]
          (when (not= ch abort-chan)
            (let [new-solving val
                  better? (< (:train-value (:best-case new-solving))
                             (:train-value current-case))
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
                  (debug "Finished, could not find better solution"))))))))))

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