(ns asols.network
  (:require [clojure.core.matrix :as mat]
            [clojure.core.matrix :as m]))

(mat/set-current-implementation :vectorz)


(defn- rand-weight [inputs-count]
  "Returns randon number in range [-scale .. scale] where scale
  is sqrt(3 / inputs-count)"
  (let [scale (Math/sqrt (/ 3 inputs-count))]
    (-> (rand)
        (* 2 scale)
        (- scale))))

;; Layer

(defn- make-edges-matrix
  [in-count out-count]
  (if (or (zero? in-count) (zero? out-count))
    [[]]
    (for [_ (range in-count)]
      (for [_ (range out-count)]
        (rand-weight in-count)))))

(defn- make-edges-mask
  [in-count out-count]
  (if (or (zero? in-count) (zero? out-count))
    [[]]
    (for [_ (range in-count)]
      (repeat out-count 1.0))))

(defn- layer
  [type in-count out-count]
  {:type         type
   :edges-matrix (mat/matrix (make-edges-matrix in-count out-count))
   :edges-mask   (mat/matrix (make-edges-mask in-count out-count))
   :biases       (mat/zero-vector out-count)})

(defn get-layer-shape
  "Returns [in-count out-count] of given layer"
  [layer]
  [(m/dimension-count (:edges-matrix layer) 0)
   (m/dimension-count (:edges-matrix layer) 1)])

(defn get-layer-edge
  "Returns edge's weight between given nodes."
  [layer node-from node-to]
  (mat/mget (:edges-matrix layer) node-from node-to))

;; Network

(defn network
  "Creates network of input and output layer."
  [in-count out-count out-type]
  {:layers [(layer out-type in-count out-count)]})

(defn for-dataset
  [dataset out-type]
  (let [{:keys [inputs-count outputs-count]} dataset]
    (network inputs-count outputs-count out-type)))

(defn shape
  "Returns network shape ([inputs-count outputs-count])"
  [{:keys [layers]}]
  [(first (get-layer-shape (first layers)))
   (second (get-layer-shape (last layers)))])

(defn- rand-net-weight [net]
  (let [[in-count _] (shape net)]
    (rand-weight in-count)))

(defn- safe-join [m1 m2]
  (m/clone (m/join m1 m2)))

(defn- safe-join-along [dim m1 m2]
  (m/clone (m/join-along dim m1 m2)))

(defn- add-node-layer-row
  [net layer-i]
  (if-not (< layer-i (count (:layers net)))
    net
    (update-in net [:layers layer-i]
              (fn [{:keys [edges-matrix] :as layer}]
                (let [row-len (m/dimension-count edges-matrix 1)
                      edges-row [(for [_ (range row-len)]
                                   (rand-net-weight net))]
                      mask-row [(repeat row-len 1.0)]]
                  (-> layer
                      (update-in [:edges-matrix] safe-join edges-row)
                      (update-in [:edges-mask] safe-join mask-row)))))))

(defn- add-node-layer-col
  [net layer-i]
  (if-not (pos? layer-i)
    net
    (update-in net [:layers (dec layer-i)]
               (fn [{:keys [edges-matrix] :as layer}]
                 (let [col-len (m/dimension-count edges-matrix 0)
                       edges-col (for [_ (range col-len)]
                                   [(rand-net-weight net)])
                       mask-col (repeat col-len [1.0])]
                   (-> layer
                       (update-in [:edges-matrix] #(safe-join-along 1 % edges-col))
                       (update-in [:edges-mask] #(safe-join-along 1 % mask-col))
                       (update-in [:biases] m/join [0])))))))

(defn add-node
  "Adds new fully-connected node to given layer, returns new network."
  [net layer-i]
  (-> net
      (add-node-layer-row layer-i)
      (add-node-layer-col layer-i)))

(defn- drop-row
  [matrix row-i]
  (let [row-count (m/dimension-count matrix 0)
        selected-rows (remove #(= row-i %) (range row-count))]
    (m/matrix (m/select matrix selected-rows :all))))

(defn- drop-col
  [matrix col-i]
  (let [col-count (m/dimension-count matrix 1)
        selected-cols (remove #(= col-i %) (range col-count))]
    (m/matrix (m/select matrix :all selected-cols))))

(defn- drop-item
  [vector item-i]
  (let [items-count (m/dimension-count vector 0)
        selected-items (remove #(= item-i %) (range items-count))]
    (m/array (m/select vector selected-items))))

(defn- del-node-layer-row
  [net layer-i node-i]
  (if-not (< layer-i (count (:layers net)))
    net
    (update-in net [:layers layer-i]
               (fn [layer]
                 (-> layer
                     (update-in [:edges-matrix] drop-row node-i)
                     (update-in [:edges-mask] drop-row node-i))))))

(defn- del-node-layer-col
  [net layer-i node-i]
  (if-not (pos? layer-i)
    net
    (update-in net [:layers (dec layer-i)]
               (fn [layer]
                 (-> layer
                     (update-in [:edges-matrix] drop-col node-i)
                     (update-in [:edges-mask] drop-col node-i)
                     (update-in [:biases] drop-item node-i ))))))

(defn del-node
  "Deletes node on given layer with all it's connections, returns new network."
  [net layer-i node-i]
  (-> net
      (del-node-layer-row layer-i node-i)
      (del-node-layer-col layer-i node-i)))

(defn insert-layer
  "Adds new hidden layer at given pos (before given layer).
  Assume that it deletes edges between layers at given position and
  new layer is fully connected between surrounding ones."
  [{:keys [layers] :as net} layer-pos layer-type nodes-count]
  {:pre [(pos? layer-pos)
         (<= layer-pos (count layers))]}
  (let [[prev-layers [next-layer & other-layers]] (split-at (dec layer-pos) layers)
        [in-count out-count] (get-layer-shape next-layer)
        new-layer (layer layer-type in-count nodes-count)
        new-next-layer (layer (:type next-layer) nodes-count out-count)
        new-layers (concat prev-layers
                           [new-layer new-next-layer]
                           other-layers)]
    (assoc net :layers (vec new-layers))))

(defn set-layer-edges
  [net layer-i edges-matrix]
  {:pre [(pos? layer-i)
         (<= layer-i (count (:layers net)))]}
  (assoc-in net [:layers (dec layer-i) :edges-matrix] edges-matrix))

(defn- set-edge
  [net layer-from node-from node-to weight mask]
  (let [new-layer (-> (get-in net [:layers layer-from])
                      (update-in [:edges-matrix] mat/mset node-from node-to weight)
                      (update-in [:edges-mask] mat/mset node-from node-to mask))]
    (assoc-in net [:layers layer-from] new-layer)))

(defn add-edge
  "Adds edge between given nodes, returns new network.
  Overrides existing edges."
  ([net layer-from node-from node-to]
    (add-edge net layer-from node-from node-to (rand-net-weight net)))
  ([net layer-from node-from node-to weight]
    (set-edge net layer-from node-from node-to weight 1.0)))

(defn edge-exists?
  [net layer-from node-from node-to]
  (-> (get-in net [:layers layer-from :edges-mask])
      (mat/mget node-from node-to)
      (double)
      (= 1.0)))

(defn get-weight
  "Returns weight of edge between nodes"
  [net layer-from node-from node-to]
  (let [layer (get-in net [:layers layer-from])]
    (get-layer-edge layer node-from node-to)))

(defn set-weight
  "Sets weight of edge between nodes, returns new network."
  [net layer-from node-from node-to new-weight]
  (set-edge net layer-from node-from node-to new-weight 1.0))

(defn del-edge
  "Deletes given edge, returns new network."
  [net layer-from node-from node-to]
  (set-edge net layer-from node-from node-to 0.0 0.0))

(defn serialize
  "Converts inner network matrixes to native Clojure data types."
  [net]
  (let [new-layers (for [layer (:layers net)]
                     (-> layer
                         (update-in [:edges-matrix] m/to-nested-vectors)
                         (update-in [:edges-mask] m/to-nested-vectors)
                         (update-in [:biases] m/to-nested-vectors)))]
    (assoc net :layers (vec new-layers))))