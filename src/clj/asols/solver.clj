(ns asols.solver
  (:require [clojure.core.async :refer [<! >! <!! >!! chan close! alts!! timeout go go-loop]]
            [clojure.core.matrix.stats :refer [mean variance]]
            [asols.network :as network]
            [asols.trainer :as trainer]
            [asols.mutations :as mutations]
            [asols.commands :as commands]
            [asols.graphics :as graphics]
            [asols.data :as data]))

(defmacro time-it
  [expr]
  `(let [start# (System/nanoTime)
         ret# ~expr
         elapsed# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     [ret# elapsed#]))

(defrecord Solver [dataset train-opts mutation-opts])

(defn create-start-net
  [{:keys [dataset train-opts mutation-opts]}]
  (let [{:keys [inputs-count outputs-count]} dataset
        hidden-type (:hidden-type mutation-opts)
        out-type (:out-type mutation-opts)
        [net h1] (-> (network/network inputs-count outputs-count out-type)
                     (network/add-layer hidden-type)
                     (network/add-node 1))]
    (-> net
        (network/full-connect 1 h1)
        (trainer/train dataset train-opts))))

(defn- get-mutations
  [{:keys [mutation-opts]} net]
  (let [{:keys [remove-nodes? remove-edges? add-layers?]} mutation-opts]
    (concat
      (mutations/identity-mutations net)
      (mutations/add-edges-mutations net)
      (mutations/add-neurons-mutations net)
      (when remove-nodes? (mutations/remove-neurons-mutations net))
      (when remove-edges? (mutations/remove-edges-mutations net))
      (when add-layers? (mutations/add-layers-mutations net)))))

(defn- calc-train-error
  [solver net]
  (trainer/calc-error net (:train (:dataset solver))))

(defn- calc-test-error
  [solver net]
  (trainer/calc-error net (:test (:dataset solver))))

(defn solve-mutation
  [solver {net :network :as mutation}]
  (let [graph (graphics/render-network net)
        trained-net (trainer/train net (:dataset solver) (:train-opts solver))
        train-error (calc-train-error solver trained-net)
        test-error (calc-test-error solver trained-net)
        new-mutation (assoc mutation :network trained-net)]
    (commands/->SolvingCase new-mutation train-error test-error graph)))

(defn solve-net
  [solver net progress-chan abort-chan]
  (let [mutations (get-mutations solver net)
        cases (for [i (range (count mutations))
                    :let [m (nth mutations i)
                          case (solve-mutation solver m)
                          progress {:mutation m
                                    :value (/ (inc i) (count mutations))}
                          [abort? _] (alts!! [abort-chan] :default false)]
                    :while (not abort?)]
                (do
                  (>!! progress-chan progress)
                  case))
        [[[best-case] cases] ms-took] (time-it (split-at 1 (sort-by :test-error cases)))]
    (commands/->Solving best-case cases ms-took)))

(defn solver-loop
  [solver out-chan abort-chan]
  (let [progress-chan (chan)
        loop-abort-chan (chan)]
    (go-loop [{:keys [mutation value]} (<! progress-chan)]
      (when-not (nil? value)
        (when (>! out-chan (commands/progress mutation value))
          (recur (<! progress-chan)))))
    (loop [current-net (create-start-net solver)
           current-error (calc-test-error solver current-net)]
      (let [solving-chan (go (solve-net solver current-net progress-chan loop-abort-chan))
            [solving ch] (alts!! [abort-chan solving-chan])]
        (if (= ch abort-chan)
          (>!! loop-abort-chan true)
          (let [{:keys [test-error mutation]} (:best-case solving)]
            (if (and (< test-error current-error)
                     (> test-error 1E-4))
              (when (>!! out-chan (commands/step solving))
                (prn "Sent step")
                (recur (:network mutation) test-error))
              (when (>!! out-chan (commands/finished solving))
                (prn "Finished")))))))))

(defn init [in-chan out-chan]
  (let [abort-chan (chan)
        init-cmd (commands/init (trainer/hidden-types) (trainer/out-types))]
    (go
      (>! out-chan init-cmd)
      (loop [command (<! in-chan)]
        (when-not (nil? command)
          (prn "Recieved command:" command)
          (case (:command command)
            ::commands/start (let [{:keys [train-opts mutation-opts]} command
                                   solver (->Solver data/monks1 train-opts mutation-opts)]
                               (go (solver-loop solver out-chan abort-chan)))
            ::commands/abort (>!! abort-chan true)
            :default (prn "Unknown command:" command))
          (recur (<! in-chan)))))))