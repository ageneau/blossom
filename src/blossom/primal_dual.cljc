(ns blossom.primal-dual
  (:require [blossom.blossom :as blossom]
            [blossom.constants :as c]
            [blossom.dual :as dual]
            [blossom.endpoint :as endp]
            [blossom.graph :as graph]
            [blossom.label :as label]
            [blossom.options :as options]
            [blossom.utils :as u]))

(defprotocol PPrimalDual
  (compute-delta-1 [context]
    "Minimum value of any vertex dual.")
  (compute-delta-2 [context]
    "Minimum slack on any edge between an S-vertex and a free vertex.")
  (compute-delta-3 [context]
    "half the minimum slack on any edge between a pair of S-blossoms.")
  (compute-delta-4 [context]
    "Minimum z variable of any T-blossom.")
  (update-best-edges [context b])
  (compute-delta [context]
    "Compute delta and reduce slack in the optimization problem.
  (Note that our vertex dual variables, edge slacks and delta's
  are pre-multiplied by two)."))

(defprotocol PPrimalDualImpl
  (get-least-slack-edges [context bv])
  (compute-my-best-edges [context b])
  (update-blossom-dual-with-delta [context delta])
  (update-dual-var-with-delta [context delta]))

(extend-type blossom.context.Context
  PPrimalDualImpl
  (get-least-slack-edges
    [context bv]
    (or (dual/blossom-best-edges context bv)
        ;; This subblossom does not have a list of least-slack edges;
        ;; get the information from the vertices.
        (for [v (blossom/leaves context bv)
              p (endp/vertex-endpoints context v)]
          (endp/edge context p))))

  (compute-my-best-edges
    [context b]
    (reduce
      (fn [best-edge-to bv]
        (reduce (fn [best-edge-to k]
                  (let [edge               (graph/edge context k)
                        j                  (if (= b (blossom/in-blossom context (graph/dest edge)))
                                             (graph/src edge)
                                             (graph/dest edge))
                        bj                 (blossom/in-blossom context j)
                        {:keys [slack-bj]} (get best-edge-to bj)]
                    (cond-> best-edge-to
                      (and (not= bj b)
                           (label/s-blossom? context bj)
                           (or (nil? slack-bj)
                               (< (dual/slack context k)
                                  slack-bj)))
                      (assoc bj {:edge  k
                                 :slack (dual/slack context k)}))))
                best-edge-to
                (get-least-slack-edges context bv)))
      (sorted-map)
      (blossom/childs context b)))

  (update-dual-var-with-delta
    [context delta]
    (loop [context context
           gnodes  (blossom/vertex-range context)]
      (if (seq gnodes)
        (let [v  (first gnodes)
              bv (blossom/in-blossom context v)]
          (recur (cond-> context
                   (label/s-blossom? context bv)
                   ;; S-vertex: 2*u = 2*u - 2*delta
                   (update-in [:dual-var v] - delta)

                   (label/t-blossom? context bv)
                   ;; T-vertex: 2*u = 2*u + 2*delta
                   (update-in [:dual-var v] + delta))
                 (next gnodes)))
        context)))

  (update-blossom-dual-with-delta
    [context delta]
    (reduce
      (fn [context b]
        (let [base-b   (blossom/base context b)
              parent-b (blossom/parent context b)]
          (cond-> context
            (and (graph/some-node? base-b)
                 (graph/no-node? parent-b)
                 (label/s-blossom? context b))
            ;; top-level S-blossom: z = z + 2*delta
            (update-in [:dual-var b] + delta)

            (and (graph/some-node? base-b)
                 (graph/no-node? parent-b)
                 (label/t-blossom? context b))
            ;; top-level T-blossom: z = z - 2*delta
            (update-in [:dual-var b] - delta))))
      context
      (blossom/blossom-range context)))

  PPrimalDual
  (compute-delta-1
    [context]
    {:delta (->> (seq (:dual-var context))
                 (take (:nvertex context))
                 (reduce min))})

  (compute-delta-2
    [context]
    (->> (blossom/vertex-range context)
         (map (fn [v]
                (let [edge (dual/best-edge context v)]
                  (when (and (label/unlabeled? context
                                               (blossom/in-blossom context v))
                             (graph/some-edge? edge))
                    {:delta-edge edge
                     :delta      (dual/slack context edge)}))))
         (u/filter-and-find-min-for-key :delta)))

  (compute-delta-3
    [context]
    (->> (range (* 2 (:nvertex context)))
         (map (fn [b]
                (let [parent-b    (blossom/parent context b)
                      best-edge-b (dual/best-edge context b)]
                  (when (and (graph/no-node? parent-b)
                             (label/s-blossom? context b)
                             (graph/some-edge? best-edge-b))
                    {:delta-edge best-edge-b
                     :delta      (/ (dual/slack context best-edge-b) 2)}))))
         (u/filter-and-find-min-for-key :delta)))

  (compute-delta-4
    [context]
    (->> (blossom/blossom-range context)
         (map (fn [b]
                (let [base-b     (blossom/base context b)
                      parent-b   (blossom/parent context b)
                      dual-var-b (dual/dual-var context b)]
                  (when (and (graph/some-node? base-b)
                             (graph/no-node? parent-b)
                             (label/t-blossom? context b))
                    {:delta         dual-var-b
                     :delta-blossom b}))))
         (u/filter-and-find-min-for-key :delta)))

  (update-best-edges [context b]
    (let [context (reduce (fn [context bv]
                            (-> context
                                (dual/blossom-best-edges-clear bv)
                                (dual/best-edge-clear bv)))
                          context
                          (blossom/childs context b))
          
          best-edges-and-slack (compute-my-best-edges context b)
          best-edges           (mapv :edge (vals best-edges-and-slack))

          my-best-edge (if (empty? best-edges-and-slack)
                         c/NO-EDGE
                         (:edge (reduce (partial min-key :slack)
                                        (reverse (vals best-edges-and-slack)))))]
      (-> context
          (dual/set-best-edge b my-best-edge)
          (dual/set-blossom-best-edges b best-edges))))

  (compute-delta
    [context]
    (let [max-cardinality (options/get-option context :max-cardinality)
          deltas          (->> context
                               ((juxt compute-delta-1
                                      compute-delta-2
                                      compute-delta-3
                                      compute-delta-4))
                               (map-indexed (fn [idx delta]
                                              (assoc delta :delta-type (inc idx)))))

          delta (if-not max-cardinality
                  (->> deltas
                       (filter :delta)
                       reverse
                       (reduce (partial min-key :delta)))
                  (let [min-d (->> deltas
                                   (drop 1) ; no delta-1
                                   (filter :delta)
                                   reverse)]
                    (if (seq min-d)
                      (reduce (partial min-key :delta) min-d)
                      ;; No further improvement possible; max-cardinality optimum
                      ;; reached. Do a final delta update to make the optimum
                      ;; verifyable.
                      (let [delta-1 (:delta (first deltas))]
                        {:delta-type 1
                         :delta      (max 0 delta-1)}))))]
      (assoc delta :context (-> context
                                (update-dual-var-with-delta (:delta delta))
                                (update-blossom-dual-with-delta (:delta delta)))))))
