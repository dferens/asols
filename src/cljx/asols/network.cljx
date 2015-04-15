(ns asols.network)

(def ^:private alphabet (map char (range 97 123)))
(def ^:private last-node-id (atom -1))

(defn- convert-base
  [num to-base]
  (if (zero? num)
    (list 0)
    (loop [num num
           digits ()]
      (if (pos? num)
        (recur (quot num to-base)
               (conj digits (mod num to-base)))
        digits))))

(defn- node
  "Creates new node:
    :a, :b, ..., :z, :ba, :bb, ..., :zz, :baa etc."
  []
  (let [last-id (swap! last-node-id inc)]
    (->> (convert-base last-id (count alphabet))
         (map (partial nth alphabet))
         (apply str)
         (keyword))))

(defprotocol NetworkProtocol
  (layers
    [this]
    "Returns collection of network layers where first one is input and last one
    is output layers")
  (in-edges [this node])
  (out-edges [this node])
  (has-edge? [this node-from node-to])
  (single-edge? [this edge])
  (get-weight
    [this edge]
    "Returns weight of given edge"))

(defrecord Layer [type nodes])

(defrecord Network [input-layer output-layer hidden-layers edges]
  NetworkProtocol
  (layers [_]
    (concat
      [input-layer]
      hidden-layers
      [output-layer]))
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
    (let [layer-from (->> (map :nodes (layers this))
                          (filter #(some #{node-from} %))
                          (first))
          layer-to (->> (map :nodes (layers this))
                        (filter #(some #{node-to} %))
                        (first))
          edges-between (for [node-a layer-from
                              node-b layer-to
                              :when (has-edge? this node-a node-b)]
                          [node-a node-b])]
      (nil? (second edges-between))))
  (get-weight [_ edge]
    (edges edge)))

(defn- rand-weight [{input-layer :input-layer}]
  "Returns randon number in range [-scale .. scale] where scale
  is sqrt(3 / inputs-count)"
  (let [scale (Math/sqrt (/ 3 (count (:nodes input-layer))))]
    (-> (rand)
        (* 2 scale)
        (- scale))))

(defn add-layer
  "Adds new hidden layer to network, returns new network"
  ([network type]
    (add-layer network 0 type))
  ([network index type]
   (let [[before-layers after-layers] (split-at index (:hidden-layers network))
         new-layer (->Layer type #{})
         new-layers (concat before-layers [new-layer] after-layers)]
     (assoc network :hidden-layers (vec new-layers)))))

(defn add-node
  "Adds new node to `layer-i`th hidden layer of network, returns new network
  and added node"
  ([network]
    (add-node network 0))
  ([network layer-i]
   (let [new-node (node)
         new-net (update-in network [:hidden-layers layer-i :nodes] conj new-node)]
     [new-net new-node])))

(defn del-node
  "Removes given node from network, returns new network"
  [{edges :edges :as network} layer-i node]
  (let [edges-left (for [edge (keys edges)
                         :when (every? #(not= % node) edge)]
                     edge)]
    (-> network
        (update-in [:hidden-layers layer-i :nodes] disj node)
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
  [network node target-layer-i]
  (let [target-layer (nth (layers network) (inc target-layer-i))
        target-layer-nodes (into #{} (:nodes target-layer))
        edges-to-move (->> (in-edges network node)
                           (remove (fn [[node _]] (target-layer-nodes node))))
        [new-net new-node] (add-node network target-layer-i)
        edge-mover (fn [net e] (move-edge net e [(first e) new-node]))
        edge-adder (fn [net node-to] (add-edge net new-node node-to))
        next-layer (nth (layers new-net) (+ target-layer-i 2))]
    (as-> new-net $
          (reduce edge-mover $ edges-to-move)
          (reduce edge-adder $ (:nodes next-layer)))))

(defn reset-weights
  [network]
  (let [new-edges (for [[k _] (:edges network)]
                    [k (rand-weight network)])]
    (assoc network :edges (into {} new-edges))))

(defn network
  "Creates new network"
  [input-count output-count output-type]
  (let [input-layer (->Layer ::input (into [] (repeatedly input-count node)))
        output-layer (->Layer output-type (into [] (repeatedly output-count node)))]
    (->Network input-layer output-layer [] {})))