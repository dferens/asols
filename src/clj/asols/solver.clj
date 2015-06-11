(ns asols.solver
  (:require [clojure.core.async :refer [<! >! >!! chan alts!! go go-loop]]
            [clojure.core.matrix :refer [transpose join-along]]
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

(defn- converged?
  [_ solving]
  (< (:train-cost (:best-case solving))
     1E-4))

(defn- sort-cases [cases]
  (sort-by :train-cost cases))

(defn create-solver
  [t-opts m-opts]
  {:train-opts t-opts
   :mutation-opts m-opts})

(defn- get-dataset [solver]
  (data/datasets (:dataset (:mutation-opts solver))))

(defn- get-train-entries [solver]
  (-> (:train (get-dataset solver))))

(defn- get-rand-train-entries [solver]
  (let [train-entries (-> (get-train-entries solver)
                          (shuffle)
                          (data/split-proportion (:train-frac (:mutation-opts solver)))
                          (first))
        validation-entries (get-train-entries solver)]
    [train-entries validation-entries]))

(defn get-test-entries
  [solver]
  (:test (get-dataset solver)))

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

(defn- calc-metrics
  "Returns vector of train cost, test cost, train ca and test ca."
  [net validation-entries test-entries]
  [(trainer/calc-cost net validation-entries)
   (trainer/calc-cost net test-entries)
   (trainer/calc-ca net validation-entries)
   (trainer/calc-ca net test-entries)])

(defn solve-mutation
  "Most important fn here.
  Tests given mutation"
  [{t-opts :train-opts :as solver} prev-net mutation]
  (let [mutated-net (m/mutate prev-net mutation)
        [train-e validation-e] (get-rand-train-entries solver)
        trained-net (trainer/train mutated-net train-e t-opts)
        metrics (calc-metrics trained-net validation-e (get-test-entries solver))
        [train-cost test-cost train-ca test-ca] metrics]
    (commands/->SolvingCase
      trained-net mutation
      train-cost test-cost
      train-ca test-ca)))

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
  [{m-opts :mutation-opts :as solver} net cases]
  (let [selected-cases (->> cases
                            (sort-cases)
                            (reverse)
                            (drop-while #(not= ::m/identity (:operation (:mutation %))))
                            (reverse)
                            (take (:max-combined-count m-opts)))]
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
          combined-cases (make-combined-cases solver net cases)
          [best-case & other-cases] (if (empty? combined-cases)
                                      (filter #(= ::m/identity (:operation (:mutation %))) cases)
                                      (sort-cases combined-cases))]
      (make-solving solver net best-case other-cases ms-took))
    (catch InterruptedException _
      (debug "Detected thread interrupt"))))

(defn create-start-net
  [{:keys [mutation-opts]}]
  (let [{:keys [hidden-type out-type hidden-count]} mutation-opts
        dataset (data/datasets (:dataset mutation-opts))]
    (-> (network/for-dataset dataset out-type)
        (network/insert-layer 1 hidden-type hidden-count))))

(defn- get-initial-train-opts
  [{t-opts :train-opts m-opts :mutation-opts}]
  (assoc t-opts :iter-count (:initial-iter-count m-opts)))

(defn create-initial-solving [solver]
  (let [net (create-start-net solver)
        train-opts (get-initial-train-opts solver)
        initial-solver (assoc solver :train-opts train-opts)
        mutation (first (m/identity-mutations net))
        case (solve-mutation initial-solver net mutation)]
    (commands/->Solving net
                        train-opts
                        (:mutation-opts solver)
                        case
                        nil
                        0)))

(defn- calc-static-metrics
  [solver net iter-count]
  (let [train-e (get-train-entries solver)
        test-e (get-test-entries solver)
        train-opts (assoc (:train-opts solver) :iter-count iter-count)
        [_ results] (trainer/train net train-e train-opts #(calc-metrics % train-e test-e))]
    (for [metrics-serie (transpose results)]
      (into {} (for [[i val] (map-indexed vector metrics-serie)]
                 [(inc i) val])))))

(defn solver-loop
  [solver out-chan abort-chan]
  (let [initial-solving (create-initial-solving solver)
        initial-static-net (:net (:best-case initial-solving))]
    (cpool/with-shutdown!
      [tpool (cpool/threadpool (cpool/ncpus))]
      (loop [current-solving initial-solving
             iter-count 0]
        (let [current-net (:net (:best-case current-solving))
              solving-chan (go (solve-net solver current-net out-chan tpool))
              [val ch] (alts!! [abort-chan solving-chan])]
          (when (not= ch abort-chan)
            (let [new-solving val
                  better? (< (:train-cost (:best-case new-solving))
                             (:train-cost (:best-case current-solving)))
                  best? (converged? solver new-solving)
                  new-iter-count (+ iter-count (:iter-count (:train-opts new-solving)))
                  metrics (calc-static-metrics solver initial-static-net new-iter-count)]
              (if better?
                (do
                  (>!! out-chan (commands/step new-solving metrics))
                  (debug "Sent step")
                  (if best?
                    (when (>!! out-chan (commands/finished))
                      (debug "Finished, found best solution"))
                    (recur new-solving
                           new-iter-count)))
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
