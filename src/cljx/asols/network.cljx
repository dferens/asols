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

(defn- edge-layer
  []
  [])

(defn- hidden-layer
  "Hidden layer is simply set of nodes"
  []
  #{})

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
    (let [layer-from (->> (layers this)
                          (filter #(some #{node-from} %))
                          (first))
          layer-to (->> (layers this)
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
  (let [scale (Math/sqrt (/ 3 (count input-layer)))]
    (-> (rand)
        (* 2 scale)
        (- scale))))

(defn network
  "Creates new network"
  [input-count output-count]
  (->Network
    (into (edge-layer) (repeatedly input-count node))
    (into (edge-layer) (repeatedly output-count node))
    []
    {}))

(defn add-layer
  "Adds new hidden layer to network, returns new network"
  [network]
  (update-in network [:hidden-layers] conj (hidden-layer)))

(defn add-node
  "Adds new node to `layer-i`th hidden layer of network, returns new network
  and added node"
  ([network]
    (add-node network 0))
  ([network layer-i]
   (let [new-node (node)
         new-net (update-in network [:hidden-layers layer-i] conj new-node)]
     [new-net new-node])))

(defn del-node
  "Removes given node from network, returns new network"
  [network layer-i node]
  (update-in network [:hidden-layers layer-i] dissoc node))

(defn add-edge
  "Adds new edge to network, returns new network"
  [network node-from node-to]
  (update-in network [:edges] assoc [node-from node-to] (rand-weight network)))

(defn set-weight
  "Sets new weight for given edge, returns new network"
  [network edge weight]
  (update-in network [:edges] assoc edge weight))

(defn del-edge
  "Removes edge from network, returns new network"
  [network edge]
  (update-in network [:edges] dissoc edge))

(defn reset-weights
  [network]
  (let [new-edges (for [[k _] (:edges network)]
                    [k (rand-weight network)])]
    (assoc network :edges (into {} new-edges))))
