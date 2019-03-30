(ns blossom.graph
  (:require [loom.graph :as lg]
            [blossom.context]
            [clojure.spec.alpha :as s]))

(def NO-NODE -1)
(def no-node? #(= NO-NODE %))
(def some-node? #(not= NO-NODE %))

(def NO-EDGE -1)
(def no-edge? #(= NO-EDGE %))
(def some-edge? #(not= NO-EDGE %))

(defprotocol PGraph
  (edges [g k])
  (endpoint [g p])
  (neighbend [g v]))

(extend-type blossom.context.Context
  PGraph
  (edges [g k]
    (nth (:edges g) k))

  (endpoint [g p]
    (nth (:endpoint g) p))

  (neighbend [g v]
    (nth (:neighbend g) v)))

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

(defn max-weight [g]
  (->> g
       :edges
       (map last)
       (reduce max)))

(defn integer-weights? [edges]
  (s/valid? (s/coll-of int?) (map last edges)))

(defn compute-endpoint [edges]
  (vec (mapcat (fn [[i j wt]]
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
  (let [[nedge max-vertex-id endpoint neighbend integer-weights?]
        ((juxt count max-vertex-id compute-endpoint compute-neighbend integer-weights?) edges)
        nvertex (inc max-vertex-id)]
    {:edges edges
     :nedge nedge
     :nvertex nvertex
     :endpoint endpoint
     :neighbend neighbend
     :integer-weights? integer-weights?}))
