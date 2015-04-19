(ns asols.network)


(defn next-node-id
  ([] (next-node-id (str (char 96))))
  ([node-id]
   (let [alphabet (range 97 123)
         id-vec (map int (name node-id))
         next-id-vec (loop [result []
                            nums-left (reverse id-vec)
                            carry 1]
                       (if (empty? nums-left)
                         (if (zero? carry)
                           result
                           (conj result (first alphabet)))
                         (let [curr-num (+ carry (first nums-left))]
                           (if (>= curr-num (last alphabet))
                             (recur (conj result (first alphabet))
                                    (rest nums-left)
                                    1)
                             (recur (conj result curr-num)
                                    (rest nums-left)
                                    0)))))]
     (->> (reverse next-id-vec)
          (map char)
          (apply str)
          (keyword)))))

(defprotocol NetworkProtocol
  (hidden-layers [this])
  (in-edges [this node])
  (out-edges [this node])
  (has-edge? [this node-from node-to])
  (single-edge? [this edge])
  (get-weight
    [this edge]
    "Returns weight of given edge"))

(defrecord Layer [type nodes])

(defn in-layer
  ([] (in-layer []))
  ([nodes]
   (->Layer ::input (vec nodes))))

(defn hidden-layer [type]
  (->Layer type #{}))

(defn out-layer
  ([type]
    (out-layer type []))
  ([type nodes]
   (->Layer type (vec nodes))))

(defrecord Network [layers edges next-node-id]
  NetworkProtocol
  (hidden-layers [_]
    (rest (butlast layers)))
  (in-edges [_ node]
    (for [edge (keys edges)
          :let [[_ node-to] edge]
          :when (= node-to node)]
      edge))
  (out-edges [_ node]
    (for [edge (keys edges)
          :let [[node-from _] edge]
          :when (= node-from node)]
      edge))
  (has-edge? [_ node-from node-to]
    (contains? edges [node-from node-to]))
  (single-edge? [this [node-from node-to]]
    (let [layer-from (->> (map :nodes layers)
                          (filter #(some #{node-from} %))
                          (first))
          layer-to (->> (map :nodes layers)
                        (filter #(some #{node-to} %))
                        (first))
          edges-between (for [node-a layer-from
                              node-b layer-to
                              :when (has-edge? this node-a node-b)]
                          [node-a node-b])]
      (nil? (second edges-between))))
  (get-weight [_ edge]
    (edges edge)))

(defn- rand-weight [{layers :layers}]
  "Returns randon number in range [-scale .. scale] where scale
  is sqrt(3 / inputs-count)"
  (let [scale (Math/sqrt (/ 3 (count (:nodes (first layers)))))]
    (-> (rand)
        (* 2 scale)
        (- scale))))

(defn add-layer
  "Adds new hidden layer to network, returns new network"
  ([network type]
    (add-layer network type 1))
  ([network type index]
   {:pre [(< 0 index (count (:layers network)))]}
   (let [[before-layers after-layers] (split-at index (:layers network))
         new-layer (hidden-layer type)
         new-layers (concat before-layers [new-layer] after-layers)]
     (assoc network :layers (vec new-layers)))))

(defn add-node
  "Adds new node to `layer-i`th layer of network, returns new network
  and added node"
  ([network]
    (add-node network 0))
  ([network layer-i]
   (let [new-node (:next-node-id network)
         new-net (-> network
                     (update-in [:layers layer-i :nodes] conj new-node)
                     (update-in [:next-node-id] next-node-id))]
     [new-net new-node])))

(defn del-node
  "Removes given node from network, returns new network"
  [{edges :edges :as network} layer-i node]
  (let [edges-left (for [edge (keys edges)
                         :when (every? #(not= % node) edge)]
                     edge)
        new-nodes (remove #{node} (get-in network [:layers layer-i :nodes]))]
    (-> network
        (assoc-in [:layers layer-i :nodes] (vec new-nodes))
        (update-in [:edges] select-keys edges-left))))

(defn add-edge
  "Adds new edge to network, returns new network"
  ([network node-from node-to]
    (add-edge network node-from node-to (rand-weight network)))
  ([network node-from node-to weight]
   (update-in network [:edges] assoc [node-from node-to] weight)))

(defn set-weight
  "Sets new weight for given edge, returns new network"
  [network edge weight]
  (update-in network [:edges] assoc edge weight))

(defn del-edge
  "Removes edge from network, returns new network"
  [network edge]
  (update-in network [:edges] dissoc edge))

(defn move-edge
  [network edge [node-from node-to]]
  (let [weight (get-weight network edge)]
    (-> network
        (del-edge edge)
        (add-edge node-from node-to weight))))

(defn split-node
  "Copies given node to previous layer, moves it's edges to copied
  one and connects copy node to all nodes from next layer."
  [network layer-i node]
  {:pre [(< 0 layer-i (count (:layers network)))]}
  (let [target-layer (nth (:layers network) (dec layer-i))
        target-layer-nodes (into #{} (:nodes target-layer))
        edges-to-move (->> (in-edges network node)
                           (remove (fn [[n _]] (target-layer-nodes n))))
        [new-net new-node] (add-node network (dec layer-i))
        edge-mover (fn [net e] (move-edge net e [(first e) new-node]))
        edge-adder (fn [net node-to] (add-edge net new-node node-to))
        next-layer (nth (:layers new-net) layer-i)]
    (as-> new-net $
          (reduce edge-mover $ edges-to-move)
          (reduce edge-adder $ (:nodes next-layer)))))

(defn reset-weights
  [network]
  (let [new-edges (for [[k _] (:edges network)]
                    [k (rand-weight network)])]
    (assoc network :edges (into {} new-edges))))

(defn network
  [in-count out-count out-type]
  (let [layers [(in-layer) (out-layer out-type)]
        base-net (->Network layers {} (next-node-id))
        node-adder #(fn [net _] (first (add-node net %)))]
    (as-> base-net $
          (reduce (node-adder 0) $ (range in-count))
          (reduce (node-adder 1) $ (range out-count)))))