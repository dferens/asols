(ns asols.solver
  (:require [clojure.pprint :as pp]
            [clojure.core.async :refer [<! >! >!! chan alts!! go go-loop]]
            [clojure.core.matrix.stats :refer [mean variance]]
            [com.climate.claypoole :as cpool]
            [taoensso.timbre :as timbre]
            [asols.network :as network]
            [asols.trainer :as trainer]
            [asols.mutations :as mutations]
            [asols.commands :as commands]
            [asols.graphics :as graphics]
            [asols.data :as data]))

(timbre/refer-timbre)

(def ^:private solving-cases-tpool
  (cpool/threadpool (cpool/ncpus)))

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

(defrecord ClassificationSolver [dataset train-opts mutation-opts]
  SolverProtocol
  (calc-net-value [_ net entries]
    (* 100 (trainer/calc-ca net entries)))
  (test-solving [_ solving prev-net-value]
    (let [curr-value (:test-value (:best-case solving))
          better? (> curr-value prev-net-value)
          best? (= curr-value 100.0)]
      [better? best?]))
  (sort-cases [_ cases]
    (reverse (sort-by :test-value cases))))

(defrecord RegressionSolver [dataset train-opts mutation-opts]
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

(defn- create-solver
  [t-opts m-opts]
  (let [dataset (data/datasets (:dataset m-opts))]
    (case (:mode m-opts)
     ::commands/regression (->RegressionSolver dataset t-opts m-opts)
     ::commands/classification (->ClassificationSolver dataset t-opts m-opts))))

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

(defn pmap-cancelable
  "Like pmap"
  [f & colls]
  (let [state (atom {:done-count 0 :last-result nil :aborted? false})]
    [(apply
       (partial cpool/pmap solving-cases-tpool)
       (fn [& args]
         (if (:aborted? @state)
           (debug "Detected abort")
           (let [result (apply f args)]
             (swap! state #(assoc % :done-count (inc (:done-count %))
                                    :last-result result))
             result)))
       colls)
     state]))

(defn solve-net
  [solver net progress-chan abort-chan]
  (let [mutations (get-mutations solver net)
        [cases state] (pmap-cancelable #(solve-mutation solver %) mutations)]
    (go
      (when (<! abort-chan)
        (reset! state (assoc @state :aborted? true))))
    (add-watch state :progress
               (fn [_ _ _ {aborted? :aborted? last-case :last-result done :done-count}]
                 (when-not aborted?
                   (>!! progress-chan {:mutation (:mutation last-case)
                                       :value    (/ done (count mutations))}))))
    (let [[sorted-cases ms-took] (time-it (sort-cases solver cases))
          [[best-case] other-cases] (split-at 1 sorted-cases)]
      (commands/->Solving best-case other-cases ms-took))))

(defn solver-loop
  [solver out-chan abort-chan]
  (let [progress-chan (chan 20)
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