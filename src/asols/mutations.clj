(ns asols.mutations)

(def operations
  #{:added-node :added-layer})

(defprotocol IMutation
  (get-network [this]))

(defrecord Mutation [operation network]
  IMutation
  (get-network [_] network))

(defn get-mutations
  "Returns lazy sequence of all possible network mutations like:
   [[:added-node #<Network 1>]
    [:added-layer #<Network 2>] ... ]"
  [network]
  [(->Mutation :added-node network)])