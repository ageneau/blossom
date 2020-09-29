(ns blossom.graph
  (:require [loom.graph :as lg]
            [blossom.constants :as c]
            [blossom.context]
            [clojure.spec.alpha :as s]))

(def no-node? #(= c/NO-NODE %))
(def some-node? #(not= c/NO-NODE %))

(def no-edge? #(= c/NO-EDGE %))
(def some-edge? #(not= c/NO-EDGE %))

(defprotocol PGraph
  (edge [g k]))

(defprotocol PWeightedEdge
  (src [edge] "Returns the source node of the edge")
  (dest [edge] "Returns the dest node of the edge")
  (weight [edge] "Returns the weight of the edge"))

(extend-type #?(:clj clojure.lang.IPersistentVector
                :cljs cljs.core.PersistentVector)
  PWeightedEdge
  (src [edge] (get edge 0))
  (dest [edge] (get edge 1))
  (weight [edge] (get edge 2)))

(extend-type blossom.context.Context
  PGraph
  (edge [g k]
    (nth (:edges g) k)))

(defn edges-with-weights
  ([g]
   (map #(conj % (lg/weight g %)) (lg/edges g))))

(defn star-graph
  "Return the star graph

  The star graph consists of one center node connected to n outer nodes.

  Parameters
  ----------
  n : int
      node labels are 0 to n with center 0.

  Notes
  -----
  The graph has n+1 nodes for integer n."
  [n]
  (lg/add-edges* (lg/weighted-graph)
                 (for [i (range n)]
                   [0 (inc i)])))

(defn max-vertex-id [edges]
  (->> edges
       (mapcat (fn [[i j _]] [i j]))
       (reduce max)))

(defn max-weight [edges]
  (->> edges
       (map last)
       (reduce max)))

(defn integer-weights? [edges]
  (s/valid? (s/coll-of int?) (map last edges)))

(defn compute-endpoint [edges]
  (vec (mapcat (fn [[i j _]]
                 [i j])
               edges)))

(defn compute-neighbend [edges]
  (reduce (fn [neighbend [idx [i j _]]]
            (-> neighbend
                (update i conj (inc (* 2 idx)))
                (update j conj (* 2 idx))))
          (vec (repeat (inc (max-vertex-id edges)) []))
          (map-indexed vector edges)))

(defn initialize [edges]
  (let [[nedge max-vertex-id endpoint neighbend max-weight integer-weights?]
        ((juxt count max-vertex-id compute-endpoint compute-neighbend max-weight integer-weights?) edges)
        nvertex (inc max-vertex-id)]
    {:edges edges
     :nedge nedge
     :nvertex nvertex
     :endpoint endpoint
     :neighbend neighbend
     :max-weight max-weight
     :integer-weights? integer-weights?}))
