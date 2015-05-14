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
  [expr]
  `(let [start# (System/nanoTime)
         ret# ~expr
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     [ret# elapsed#]))

(defprotocol SolverProtocol
  (calc-net-value [this net entries])
  (test-solving [this solving prev-net-value])
  (sort-cases [this cases]))

(defrecord ClassificationSolver [train-opts mutation-opts]
  SolverProtocol
  (calc-net-value [_ net entries]
    (* 100 (trainer/calc-ca net entries)))
  (test-solving [_ solving prev-net-value]
    (let [curr-value (:test-value (:best-case solving))
          better? (>= curr-value prev-net-value)
          best? (= curr-value 100.0)]
      [better? best?]))
  (sort-cases [_ cases]
    (reverse (sort-by :test-value cases))))

(defrecord RegressionSolver [train-opts mutation-opts]
  SolverProtocol
  (calc-net-value [_ net entries]
    (trainer/calc-squares-error net entries))
  (test-solving [_ solving prev-net-value]
    (let [curr-value (:test-value (:best-case solving))
          better? (< curr-value prev-net-value)
          best? (< curr-value 1E-4)]
      [better? best?]))
  (sort-cases [_ cases]
    (sort-by :test-value cases)))

(defn create-solver
  [t-opts m-opts]
  (case (:mode m-opts)
    ::commands/regression (->RegressionSolver t-opts m-opts)
    ::commands/classification (->ClassificationSolver t-opts m-opts)))

(defn get-dataset
  [solver]
  (get data/datasets (:dataset (:mutation-opts solver))))

(defn train-with
  [net solver]
  (trainer/train net (get-dataset solver) (:train-opts solver)))

(defn- make-solving
  [solver net best-case other-cases ms-took]
  (commands/->Solving net
                      (:train-opts solver)
                      (:mutation-opts solver)
                      best-case
                      other-cases
                      ms-took))

(defn create-start-net
  [{:keys [mutation-opts] :as solver}]
  (let [{:keys [inputs-count outputs-count]} (get-dataset solver)
        {:keys [hidden-type out-type hidden-count]} mutation-opts]
    (-> (network/network inputs-count outputs-count out-type)
        (network/insert-layer 1 hidden-type hidden-count)
        (train-with solver))))

(defn- get-mutations
  [{:keys [mutation-opts]} net]
  (let [{:keys [remove-nodes? remove-edges? add-layers?]} mutation-opts]
    (concat
      (mutations/identity-mutations net)
      (mutations/add-edge-mutations net)
      (mutations/add-node-mutations net)
      (when remove-nodes? (mutations/del-node-mutations net))
      (when remove-edges? (mutations/del-edge-mutations net))
      (when add-layers? (mutations/add-layers-mutations net)))))

(defn solve-mutation
  [solver {net :network :as mutation}]
  (let [mode (:mode (:mutation-opts solver))
        dataset (get-dataset solver)
        trained-net (train-with net solver)
        cost (trainer/calc-cost trained-net (:train dataset))
        new-mutation (assoc mutation :network trained-net)
        train-value (calc-net-value solver trained-net (:train dataset))
        test-value (calc-net-value solver trained-net (:test dataset))]
    (commands/->SolvingCase mode new-mutation cost train-value test-value)))

(defn solve-net
  [solver net progress-chan tpool]
  (try
    (let [mutations (get-mutations solver net)
          solve-fn (fn [[i mutation]]
                     (let [result (solve-mutation solver mutation)]
                       (go (>! progress-chan {:mutation mutation
                                              :value    (/ (inc i)
                                                           (count mutations))}))
                       result))
          cases (cpool/pmap tpool solve-fn (map-indexed vector mutations))]
      (let [[sorted-cases ms-took] (time-it (sort-cases solver cases))
            [[best-case] other-cases] (split-at 1 sorted-cases)]
        (make-solving solver net best-case other-cases ms-took)))
    (catch InterruptedException _
      (debug "Detected thread interrupt"))))

(defn solver-loop
  [solver out-chan abort-chan]
  (let [progress-chan (chan 20)
        entries (:test (get-dataset solver))]
    (go-loop [{:keys [mutation value]} (<! progress-chan)]
      (when-not (nil? value)
        (when (>! out-chan (commands/progress mutation value))
          (recur (<! progress-chan)))))
    (cpool/with-shutdown! [tpool (cpool/threadpool (cpool/ncpus))]
      (loop [current-net (create-start-net solver)
            current-value (calc-net-value solver current-net entries)]
        (let [solving-chan (go (solve-net solver current-net progress-chan tpool))
              [solving ch] (alts!! [abort-chan solving-chan])]
          (when (not= ch abort-chan)
            (let [[better? best?] (test-solving solver solving current-value)]
              (if better?
                (do
                  (>!! out-chan (commands/step solving))
                  (debug "Sent step")
                  (if best?
                    (when (>!! out-chan (commands/finished))
                      (debug "Finished, found best solution"))
                    (recur (:network (:mutation (:best-case solving)))
                           (:test-value (:best-case solving)))))
                (when (>!! out-chan (commands/finished solving))
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