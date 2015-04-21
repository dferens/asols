(ns asols.solver
  (:require [clojure.core.async :refer [<! >! chan close! go go-loop]]
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

(defprotocol SolverProtocol
  (create-start-net [this])
  (get-mutations [this net])
  (calc-train-error [this net])
  (calc-test-error [this net])
  (solve-mutation [this mutation])
  (solve-net [this net progress-chan]))

(defrecord Solver [dataset train-opts mutation-opts]
  SolverProtocol
  (create-start-net [_]
    (let [{:keys [inputs-count outputs-count]} dataset
          hidden-type (:hidden-type mutation-opts)
          out-type (:out-type mutation-opts)
          [net h1] (-> (network/network inputs-count outputs-count out-type)
                       (network/add-layer hidden-type)
                       (network/add-node 1))]
      (-> net
          (network/full-connect 1 h1)
          (trainer/train dataset train-opts))))

  (get-mutations [_ net]
    (let [{:keys [remove-nodes? remove-edges? add-layers?]} mutation-opts]
      (concat
        (mutations/identity-mutations net)
        (mutations/add-edges-mutations net)
        (mutations/add-neurons-mutations net)
        (when remove-nodes? (mutations/remove-neurons-mutations net))
        (when remove-edges? (mutations/remove-edges-mutations net))
        (when add-layers? (mutations/add-layers-mutations net)))))

  (calc-train-error [_ net]
    (trainer/calc-error net (:train dataset)))

  (calc-test-error [_ net]
    (trainer/calc-error net (:test dataset)))

  (solve-mutation [this {net :network :as mutation}]
    (let [graph (graphics/render-network net)
          trained-net (trainer/train net dataset train-opts)
          train-error (calc-train-error this trained-net)
          test-error (calc-test-error this trained-net)
          new-mutation (assoc mutation :network trained-net)]
      (commands/->SolvingCase new-mutation train-error test-error graph)))

  (solve-net [this net progress-chan]
    (let [mutations (get-mutations this net)
          cases (for [i (range (count mutations))]
                  (let [m (nth mutations i)
                        case (solve-mutation this m)
                        progress (/ (inc i) (count mutations))
                        _ (go (>! progress-chan {:mutation m :value progress}))]
                    case))
          [[[best-case] cases] ms-took] (time-it (split-at 1 (sort-by :test-error cases)))]
      (commands/->Solving best-case cases ms-took))))

(defn init [in-chan out-chan]
  (go (>! out-chan (commands/init (trainer/hidden-types) (trainer/out-types))))
  (go-loop [command (<! in-chan)]
    (when-not (nil? command)
      (prn "Recieved command:" command)
      (case (:command command)
        ::commands/start
        (let [{:keys [train-opts mutation-opts]} command
              dataset data/monks1
              solver (->Solver dataset train-opts mutation-opts)
              progress-chan (chan)
              start-net (create-start-net solver)
              start-error (calc-test-error solver start-net)]
          (go-loop [{:keys [mutation value]} (<! progress-chan)]
            (when-not (nil? value)
              (>! out-chan (commands/progress mutation value))
              (recur (<! progress-chan))))
          (loop [current-net start-net
                 current-error start-error]
            (let [solving (solve-net solver current-net progress-chan)
                  {:keys [test-error mutation]} (:best-case solving)]
              (if (and (< test-error current-error)
                       (> test-error 1E-4))
                (do
                  (>! out-chan (commands/step solving))
                  (prn "Sent step")
                  (recur (:network mutation) test-error))
                (do
                  (prn "Finished")
                  (>! out-chan (commands/finished solving)))))))
       :default
       (prn "Unknown command:" command))
      (recur (<! in-chan)))))