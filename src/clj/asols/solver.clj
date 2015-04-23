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
        node-adder (fn [net _]
                     (let [[new-net node] (network/add-node net 1)]
                       (network/full-connect new-net 1 node)))
        nodes-adder #(reduce node-adder % (range (:hidden-count mutation-opts)))]
    (-> (network/network inputs-count outputs-count out-type)
        (network/add-layer hidden-type)
        (nodes-adder)
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

(defmulti calc-net-value (fn [solver net entries] (:mode (:mutation-opts solver))))
(defmethod calc-net-value ::commands/classification
  [_ net entries]
  (* 100 (trainer/calc-ca net entries)))
(defmethod calc-net-value ::commands/regression
  [_ net entries]
  (trainer/calc-squares-error net entries))

(defmulti converged? (fn [solving-case prev-value] (:mode solving-case)))
(defmethod converged? ::commands/classification
  [solving-case prev-value]
  (let [curr-value (:test-value solving-case)]
    (or (< curr-value prev-value)
        (= curr-value 100.0))))
(defmethod converged? ::commands/regression
  [solving-case prev-value]
  (let [curr-value (:test-value solving-case)]
    (or (> curr-value prev-value)
        (< curr-value 1E-4))))

(defmulti valid-cases (fn [solver cases] (:mode (:mutation-opts solver))))
(defmethod valid-cases ::commands/regression
  [_ cases]
  (sort-by :test-value cases))
(defmethod valid-cases ::commands/classification
  [_ cases]
  (reverse (sort-by :test-value cases)))

(defn solve-mutation
  [solver {net :network :as mutation}]
  (let [mode (:mode (:mutation-opts solver))
        dataset (:dataset solver)
        graph (graphics/render-network net)
        trained-net (trainer/train net dataset (:train-opts solver))
        new-mutation (assoc mutation :network trained-net)
        train-value (calc-net-value solver trained-net (:train dataset))
        test-value (calc-net-value solver trained-net (:test dataset))]
    (commands/->SolvingCase mode new-mutation train-value test-value graph)))

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
        [[[best-case] cases] ms-took] (time-it (split-at 1 (valid-cases solver cases)))]
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
           current-value (calc-net-value solver current-net (:test (:dataset solver)))]
      (let [solving-chan (go (solve-net solver current-net progress-chan loop-abort-chan))
            [solving ch] (alts!! [abort-chan solving-chan])]
        (if (= ch abort-chan)
          (>!! loop-abort-chan true)
          (let [best-case (:best-case solving)]
            (prn (:test-value best-case) current-value)
            (if (converged? best-case current-value)
              (when (>!! out-chan (commands/finished solving))
                (prn "Finished"))
              (when (>!! out-chan (commands/step solving))
                (prn "Sent step")
                (recur (:network (:mutation best-case))
                       (:test-value best-case))))))))))

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