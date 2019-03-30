(ns blossom.dual
  (:require [blossom.constants :as c]
            [blossom.context :as ctx]
            [blossom.graph :as graph]))

(defprotocol PDualProblem
  (best-edge-assoc [this b edge])
  (best-edge [this b])
  (best-edge-clear [this b])
  (best-edge-clear-all [this])

  (blossom-best-edges-assoc [this b edge-list])
  (blossom-best-edges [this b])
  (blossom-best-edges-clear [this b])
  (blossom-best-edges-clear-all [this])

  (dual-var-assoc [this v x])
  (dual-var [this v])
  (slack [this k]
    "Return 2 * slack of edge k (does not work inside blossoms).")

  (allow-edge-assoc [this edge v])
  (allowed-edge? [this edge])
  (allow-edge-clear [this]))

(extend-type blossom.context.Context
  PDualProblem
  (best-edge-assoc [this b edge]
    (update this :best-edge assoc b edge))

  (best-edge [this b]
    (nth (:best-edge this) b))

  (best-edge-clear [this b]
    (best-edge-assoc this b c/NO-EDGE))

  (best-edge-clear-all [this]
    (assoc this :best-edge (vec (repeat (* 2 (:nvertex this)) c/NO-EDGE))))

  (blossom-best-edges-assoc [this b edge-list]
    (update this :blossom-best-edges assoc b edge-list))
  
  (blossom-best-edges [this b]
    (get (:blossom-best-edges this) b))

  (blossom-best-edges-clear [this b]
    (update this :blossom-best-edges dissoc b))

  (blossom-best-edges-clear-all [this]
    (update this :blossom-best-edges empty))

  (dual-var-assoc [this v x]
    (update this :dual-var assoc v x))

  (dual-var [this v]
    (nth (:dual-var this) v))

  (allow-edge-assoc [this edge v]
    (update this :allow-edge assoc edge v))

  (allowed-edge? [this edge]
    (nth (:allow-edge this) edge))

  (slack
    [this k]
    (let [edge (graph/edge this k)
          coerce (if (:integer-weights? this) int double)]
      (+ (coerce (dual-var this (graph/src edge)))
         (coerce (dual-var this (graph/dest edge)))
         (- (* 2 (coerce (graph/weight edge)))))))

  (allow-edge-clear [this]
    (assoc this :allow-edge (vec (repeat (:nedge this) false)))))
