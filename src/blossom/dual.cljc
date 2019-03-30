(ns blossom.dual
  (:require [blossom.context :as ctx]
            [blossom.graph :as graph]))

(defprotocol IDualProblem
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
  IDualProblem
  (best-edge-assoc [this b edge]
    (update this :best-edge assoc b edge))

  (best-edge [this b]
    (nth (:best-edge this) b))

  (best-edge-clear [this b]
    (best-edge-assoc this b graph/NO-EDGE))

  (best-edge-clear-all [this]
    (assoc this :best-edge (vec (repeat (* 2 (:nvertex this)) graph/NO-EDGE))))

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
    (let [[i j wt] (graph/edges this k)
          coerce (if (:integer-weights? this) int double)]
      (+ (coerce (dual-var this i))
         (coerce (dual-var this j))
         (- (* 2 (coerce wt))))))

  (allow-edge-clear [this]
    (assoc this :allow-edge (vec (repeat (:nedge this) false)))))
