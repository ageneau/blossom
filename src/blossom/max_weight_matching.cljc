(ns blossom.max-weight-matching
  (:require [blossom.blossom :as blossom]
            [blossom.context :as ctx]
            [blossom.constants :as c]
            [blossom.dual :as dual]
            [blossom.endpoint :as endp]
            [blossom.graph :as graph]
            [blossom.label :as label]
            [blossom.mate :as mate]
            [blossom.options :as options]
            [blossom.queue :as queue]
            [blossom.primal-dual :as pdual]
            [ageneau.utils.core :as utils]))

(defprotocol PMaxWeightMatchingImpl
  (blossom-loop-direction [ctx b entry-child])
  (act-on-minimum-delta [ctx delta-type delta-edge delta-blossom])
  (promote-sub-blossoms-to-top-blossoms [ctx b endstage])
  (recycle-blossom [ctx b])
  (scan-blossom [ctx v w]
    "Trace back from vertices `v` and `w` to discover either a new blossom
  or an augmenting path. Return the base vertex of the new blossom,
  or NO-NODE if an augmenting path was found.")
  (find-parent-blossoms [ctx b])
  (trace-to-base [ctx v bb])
  (expand-tight-sblossoms [ctx])
  (augment-blossom [ctx b v]
    "Swap matched/unmatched edges over an alternating path through blossom `b`
  between vertex `v` and the base vertex. Keep blossom bookkeeping
  consistent.")
  (find-augmenting-path [ctx])
  (immediate-subblossom-of [ctx v b]
    "Starting from a vertex `v`, ascend the blossom tree, and
  return the sub-blossom immediately below `b`.")
  (expand-blossom [ctx b endstage]
    "Expand the given top-level blossom.
  Returns an updated `context`.")
  (augment-matching [ctx k]
    "Swap matched/unmatched edges over an alternating path between two
  single vertices. The augmenting path runs through S-vertices `v` and `w`.
  Returns an updated `context`.")
  (calc-slack [ctx k]
    "Returns a map with keys kslack and context.
  kslack is the slack for edge k context is and context is an updated context
  with a modified allow-edge cache.")
  (move-to-base-relabeling [ctx b])
  (initialize-stage [ctx])
  (augment-blossom-step [ctx b j x])
  (match-endpoint [ctx p]
    "Add endpoint p's edge to the matching.")
  (assign-label [ctx w t p]
    "Assign label `t` to the top-level blossom containing vertex `w`,
  and record the fact that w was reached through the edge with
  remote enpoint `p`.
  Returns an updated `context`.")
  (verify-optimum [ctx]
    "Verify that the optimum solution has been reached.")
  (relabel-base-t-subblossom [ctx b p])
  (add-blossom [ctx base k]
    "Construct a new blossom with given `base`, containing edge k which
  connects a pair of S vertices. Label the new blossom as S; set its dual
  variable to zero; relabel its T-vertices to S and add them to the queue.
  Returns an updated `context`.")
  (move-back-to-entry-child-relabeling [ctx b])
  (scan-neighbors [ctx v])
  (entry-child [ctx b])
  (first-labeled-blossom-leaf [ctx bv])
  (consider-loose-edge-to-free-vertex
    [ctx w k kslack]
    "w is a free vertex (or an unreached vertex inside
  a T-blossom) but we can not reach it yet;
  keep track of the least-slack edge that reaches w.")
  (consider-loose-edge-to-s-blossom
    [ctx bv k kslack]
    "keep track of the least-slack non-allowable edge to
  a different S-blossom.")
  (consider-tight-edge
    [ctx p v])
  (mate-endps-to-vertices
    [ctx]
    "Transform mate[] such that mate[v] is the vertex to which v is paired. Return the updated mate[] sequence")
  (valid-matching? [ctx matching]
    "Check if the matching is symmetric"))

(extend-type blossom.context.Context
  PMaxWeightMatchingImpl
  (entry-child [ctx b]
    (->> b
         (label/endp ctx)
         (endp/opposite-vertex ctx)
         (blossom/in-blossom ctx)))

  (assign-label
    [ctx w t p]
    (let [b (blossom/in-blossom ctx w)
          base (blossom/base ctx b)]
      (assert (and (label/unlabeled? ctx w)
                   (label/unlabeled? ctx b)))
      (-> ctx
          (label/add-label w t)
          (label/add-label b t)
          (label/set-endp w p)
          (label/set-endp b p)
          (dual/best-edge-clear w)
          (dual/best-edge-clear b)
          (cond->
              (= c/S-BLOSSOM t)
            (queue/queue-push (blossom/leaves ctx b))

            (= c/T-BLOSSOM t)
            (assign-label (endp/vertex ctx (mate/mate ctx base))
                          c/S-BLOSSOM
                          (endp/opposite ctx (mate/mate ctx base)))))))

  (scan-blossom
    [ctx v w]
    ;; Trace back from v and w, placing breadcrumbs as we go.
    (loop [path []
           base c/NO-NODE
           v v
           w w
           ctx ctx]
      (if (and (graph/no-node? v)
               (graph/no-node? w))
        base
        (let [b (blossom/in-blossom ctx v)]
          ;; Look for a breadcrumb in v's blossom or put a new breadcrumb.
          (if (label/breadcrumb? ctx b)
            (blossom/base ctx b)
            (let [_ (assert (label/s-blossom? ctx b))
                  path (conj path b)
                  ctx (label/add-label ctx b c/BREADCRUMB)
                  ;; Trace one step back.
                  _ (assert (= (label/endp ctx b)
                               (mate/mate ctx (blossom/base ctx b))))
                  v (if (label/no-endp? ctx b)
                      ;; The base of blossom b is single; stop tracing this path.
                      c/NO-NODE
                      (let [v (endp/vertex ctx (label/endp ctx b))
                            b (blossom/in-blossom ctx v)]
                        (assert (label/t-blossom? ctx b))
                        ;; b is a T-blossom; trace one more step back.
                        (assert (label/some-endp? ctx b))
                        (endp/vertex ctx (label/endp ctx b))))]
              ;; Swap v and w so that we alternate between both paths.
              (if (graph/some-node? w)
                (recur path base w v ctx)
                (recur path base v w ctx))))))))

  (trace-to-base
    [ctx v bb]
    (loop [v v
           path []]
      (let [bv (blossom/in-blossom ctx v)]
        (if (= bv bb)
          path
          (do
            (assert (or (label/t-blossom? ctx bv)
                        (and (label/s-blossom? ctx bv)
                             (= (label/endp ctx bv)
                                (mate/mate ctx (blossom/base ctx bv))))))
            (assert (label/some-endp? ctx bv))
            (recur (endp/vertex ctx (label/endp ctx bv))
                   (conj path bv)))))))

  (add-blossom
    [ctx base k]
    (let [edge (graph/edge ctx k)
          v (graph/src edge)
          w (graph/dest edge)
          bb (blossom/in-blossom ctx base)

          ;; Create a new top-level blossom.
          b (blossom/unused-peek ctx)
          ctx (-> ctx
                  (blossom/unused-pop)
                  (blossom/set-base b base)
                  (blossom/remove-parent b)
                  (blossom/set-parent bb b))

          ;; Make list of sub-blossoms and their interconnecting edge endpoints.
          ;; Trace back from v to base.
          path1 (trace-to-base ctx v bb)

          ;; Trace back from w to base.
          path2 (trace-to-base ctx w bb)

          ;; Join paths, add endpoint that connects the pair of S vertices.
          path (concat [bb] (reverse path1) path2)
          endps (concat (->> path1
                             (map (partial label/endp ctx))
                             reverse)
                        [(* 2 k)]
                        (map (comp (partial endp/opposite ctx)
                                   (partial label/endp ctx))
                             path2))

          ctx (reduce (fn [ctx bv]
                        ;; Add bv to the new blossom.
                        (blossom/set-parent ctx bv b))
                      ctx
                      (concat path1 path2))

          ctx (-> ctx
                  (blossom/set-childs b path)
                  (blossom/set-endps b endps)
                  (utils/doto-assert #(label/s-blossom? % bb))
                  ;; Set label to S.
                  (label/add-label b c/S-BLOSSOM)
                  (label/set-endp b (label/endp ctx bb))
                  ;; Set dual variable to zero.
                  (dual/set-dual-var b 0))

          ;; Relabel vertices.
          ctx (reduce (fn [ctx v]
                        (cond-> ctx
                          (label/t-blossom? ctx (blossom/in-blossom ctx v))
                          ;; This T-vertex now turns into an S-vertex because it becomes
                          ;; part of an S-blossom; add it to the queue.
                          (queue/queue-push [v])

                          :true
                          (blossom/set-in-blossom v b)))
                      ctx
                      (blossom/leaves ctx b))]
      (pdual/update-best-edges ctx b)))

  (promote-sub-blossoms-to-top-blossoms
    [ctx b endstage]
    (reduce (fn [ctx s]
              (as-> ctx ctx
                (blossom/remove-parent ctx s)
                (if-not (blossom/trivial-blossom? ctx s)
                  (if (and endstage (zero? (dual/dual-var ctx s)))
                    ;; Recursively expand this sub-blossom.
                    (expand-blossom ctx s endstage)
                    (reduce (fn [ctx v]
                              (blossom/set-in-blossom ctx v s))
                            ctx
                            (blossom/leaves ctx s)))

                  (blossom/set-in-blossom ctx s s))))
            ctx
            (blossom/childs ctx b)))

  (blossom-loop-direction
    [ctx b entry-child]
    (let [entry-child-index (blossom/childs-find ctx b entry-child)]
      (if (odd? entry-child-index)
        ;; Start index is odd; go forward and wrap.
        [(- entry-child-index (blossom/childs-count ctx b)) 1 0]
        ;; Start index is even; go backward.
        [entry-child-index -1 1])))

  (move-to-base-relabeling
    [ctx b]
    (let [_ (assert (label/some-endp? ctx b))
          entry-child (entry-child ctx b)
          [j jstep endptrick] (blossom-loop-direction ctx b entry-child)]
      ;; Move along the blossom until we get to the base.
      (loop [j j
             p (label/endp ctx b)
             ctx ctx]
        (if (zero? j)
          ;; Relabel the base T-sub-blossom WITHOUT stepping through to
          ;; its mate (so don't call assignLabel).
          (relabel-base-t-subblossom ctx b p)
          (let [endp (blossom/endpoint ctx b (- j endptrick))
                ctx (-> ctx
                        ;; Relabel the T-sub-blossom.
                        (label/remove-label (endp/opposite-vertex ctx p))
                        (label/remove-label (endp/opposite-vertex ctx (bit-xor endp endptrick)))
                        (assign-label (endp/opposite-vertex ctx p) c/T-BLOSSOM p)
                        (dual/set-allow-edge (endp/edge ctx endp) true))
                ;; Step to the next S-sub-blossom and note its forward endpoint.
                j (+ j jstep)
                p (bit-xor (blossom/endpoint ctx b (- j endptrick)) endptrick)
                ctx (dual/set-allow-edge ctx (endp/edge ctx p) true)]
            ;; Step to the next T-sub-blossom.
            (recur (+ j jstep) p ctx))))))

  (first-labeled-blossom-leaf
    [ctx bv]
    (first (filter #(label/labeled? ctx %)
                   (blossom/leaves ctx bv))))

  (move-back-to-entry-child-relabeling
    [ctx b]
    ;; Start at the sub-blossom through which the expanding
    ;; blossom obtained its label, and relabel sub-blossoms untili
    ;; we reach the base.
    ;; Figure out through which sub-blossom the expanding blossom
    ;; obtained its label initially.
    (let [_ (assert (label/some-endp? ctx b))
          entry-child (entry-child ctx b)
          [_ jstep _] (blossom-loop-direction ctx b entry-child)
          j jstep]
      (loop [j j
             ctx ctx]
        ;; Examine the vertices of the sub-blossom to see whether
        ;; it is reachable from a neighbouring S-vertex outside the
        ;; expanding blossom.
        (let [bv (blossom/child ctx b j)
              v (first-labeled-blossom-leaf ctx bv)]
          (if (= bv entry-child)
            ctx
            (recur (+ j jstep)
                   (if-not (and (not (label/s-blossom? ctx bv))
                                (some? v))
                     ctx
                     (do
                       (assert (label/t-blossom? ctx v))
                       (assert (= (blossom/in-blossom ctx v) bv))
                       (-> ctx
                           (label/remove-label v)
                           (label/remove-label (endp/vertex ctx (mate/mate ctx (blossom/base ctx bv))))
                           (assign-label v c/T-BLOSSOM (label/endp ctx v)))))))))))

  (relabel-base-t-subblossom [ctx b p]
    (let [bv (first (blossom/childs ctx b))]
      (-> ctx
          (label/add-label bv c/T-BLOSSOM)
          (label/add-label (endp/opposite-vertex ctx p) c/T-BLOSSOM)
          (label/set-endp bv p)
          (label/set-endp (endp/opposite-vertex ctx p) p)
          (dual/best-edge-clear bv))))

  (recycle-blossom [ctx b]
    ;; Recycle the blossom number.
    (-> ctx
        (label/remove-label b)
        (label/remove-endp b)
        (blossom/childs-clear b)
        (blossom/endps-clear b)
        (blossom/base-clear b)
        (blossom/unused-add b)
        (dual/blossom-best-edges-clear b)
        (dual/best-edge-clear b)))

  (expand-blossom
    [ctx b endstage]
    (-> ctx
        (promote-sub-blossoms-to-top-blossoms b endstage)
        ;; If we expand a T-blossom during a stage, its sub-blossoms must be
        ;; relabeled.
        (cond->
            (and (not endstage)
                 (label/t-blossom? ctx b))
          ;; Start at the sub-blossom through which the expanding
          ;; blossom obtained its label, and relabel sub-blossoms until
          ;; we reach the base.
          ;; Figure out through which sub-blossom the expanding blossom
          ;; obtained its label initially.

          ;; Move along the blossom until we get to the base.
          (-> (move-to-base-relabeling b)
              ;; Continue along the blossom until we get back to entrychild.
              (move-back-to-entry-child-relabeling b)))

        (recycle-blossom b)))

  (immediate-subblossom-of
    [ctx v b]
    (loop [t v]
      ;; Bubble up through the blossom tree from vertex v to an immediate
      ;; sub-blossom of b.
      (let [parent (blossom/parent ctx t)]
        (if (= b parent)
          t
          (recur parent)))))

  (augment-blossom-step [ctx b j x]
    (let [t (blossom/child ctx b j)]
      (cond-> ctx
        (not (blossom/trivial-blossom? ctx t))
        (augment-blossom t x))))

  (match-endpoint
    [ctx p]
    (-> ctx
        (mate/set-mate (endp/vertex ctx p) (endp/opposite ctx p))
        (mate/set-mate (endp/opposite-vertex ctx p) p)))

  (augment-blossom
    [ctx b v]
    (let [t (immediate-subblossom-of ctx v b)
          ctx (cond-> ctx
                (not (blossom/trivial-blossom? ctx t))
                ;; Recursively deal with the first sub-blossom.
                (augment-blossom t v))

          [j jstep endptrick] (blossom-loop-direction ctx b t)
          entry-child-index j]
      ;; Move along the blossom until we get to the base.
      (loop [j j
             t t
             ctx ctx]
        (if (zero? j)
          (-> ctx
              (blossom/rotate-childs b entry-child-index)
              ;; FIXME: This should go in the post condition but this breaks CLJS
              ;; for some reason
              (utils/doto-assert #(= (blossom/base % b) v)))
          (let [;; Step to the next sub-blossom and augment it recursively.
                j (+ j jstep)
                p (bit-xor (blossom/endpoint ctx b (- j endptrick)) endptrick)
                x (endp/vertex ctx p)

                ctx (augment-blossom-step ctx b j x)

                x (endp/opposite-vertex ctx p)
                ;; Step to the next sub-blossom and augment it recursively.
                j (+ j jstep)
                ctx (augment-blossom-step ctx b j x)

                ;; Match the edge connecting those sub-blossoms.
                ctx (match-endpoint ctx p)]
            (recur j t ctx))))))

  (augment-matching
    [ctx k]
    (let [edge (graph/edge ctx k)
          v (graph/src edge)
          w (graph/dest edge)]
      (loop [ctx ctx
             permuted false
             s v
             p (inc (* 2 k))]
        ;; Match vertex s to remote endpoint p. Then trace back from s
        ;; until we find a single vertex, swapping matched and unmatched
        ;; edges as we go.
        (let [bs (blossom/in-blossom ctx s)
              _ (assert (label/s-blossom? ctx bs))
              _ (assert (= (label/endp ctx bs)
                           (mate/mate ctx (blossom/base ctx bs))))
              ctx (cond-> ctx
                    (not (blossom/trivial-blossom? ctx bs))
                    (augment-blossom bs s)

                    :true
                    ;; Update mate[s]
                    (mate/set-mate s p))]
          ;; Trace one step back.
          (if (label/no-endp? ctx bs)
            ;; Reached single vertex; try with [s p] = [w 2*k] or stop.
            (if-not permuted
              (recur ctx true w (* 2 k))
              ctx)
            (let [t (endp/vertex ctx (label/endp ctx bs))
                  bt (blossom/in-blossom ctx t)
                  _ (assert (label/t-blossom? ctx bt))
                  ;; Trace one step back.
                  s (endp/vertex ctx (label/endp ctx bt))
                  j (endp/opposite-vertex ctx (label/endp ctx bt))

                  ;; Augment through the T-blossom from j to base.
                  _ (assert (= t (blossom/base ctx bt)))
                  ctx (cond-> ctx
                        (not (blossom/trivial-blossom? ctx bt))
                        (augment-blossom bt j)

                        :true
                        ;; Update mate[j]
                        (mate/set-mate j (label/endp ctx bt)))]
              (recur ctx permuted s (endp/opposite ctx (label/endp ctx bt)))))))))

  (initialize-stage
    [ctx]
    (-> ctx
        ;; Remove labels from top-level blossoms/vertices.
        label/remove-all-labels

        ;; Forget all about least-slack edges.
        dual/best-edge-clear-all
        dual/blossom-best-edges-clear-all

        ;; Loss of labeling means that we can not be sure that currently
        ;; allowable edges remain allowable througout this stage.
        dual/allow-edge-clear
        ;; Make queue empty.
        queue/queue-clear

        ;; Label single blossoms/vertices with S and put them in the queue.
        (as-> ctx
            (reduce (fn [ctx v]
                      (cond-> ctx
                        (and (endp/no-endp? (mate/mate ctx v))
                             (label/unlabeled? ctx (blossom/in-blossom ctx v)))
                        (assign-label v c/S-BLOSSOM c/NO-ENDP)))
                    ctx
                    (blossom/vertex-range ctx)))))

  (expand-tight-sblossoms
    [ctx]
    (reduce (fn [ctx b]
              (cond-> ctx
                (and (graph/no-node? (blossom/parent ctx b))
                     (graph/some-node? (blossom/base ctx b))
                     (label/s-blossom? ctx b)
                     (zero? (dual/dual-var ctx b)))
                (expand-blossom b true)))
            ctx
            (blossom/blossom-range ctx)))

  (consider-loose-edge-to-free-vertex
    [ctx w k kslack]
    (cond-> ctx
      (or (graph/no-edge? (dual/best-edge ctx w))
          (< kslack
             (dual/slack ctx (dual/best-edge ctx w))))
      (dual/set-best-edge w k)))

  (consider-loose-edge-to-s-blossom
    [ctx bv k kslack]
    (cond-> ctx
      (or (graph/no-edge? (dual/best-edge ctx bv))
          (< kslack
             (dual/slack ctx (dual/best-edge ctx bv))))
      (dual/set-best-edge bv k)))

  (calc-slack
    [ctx k]
    (let [allowed? (dual/allowed-edge? ctx k)
          kslack (when-not allowed? (dual/slack ctx k))]
      {:kslack kslack
       :context (cond-> ctx
                  (and (not allowed?) (<= kslack 0))
                  ;; edge k has zero slack => it is allowable
                  (dual/set-allow-edge k true))}))

  (consider-tight-edge
    [ctx p v]
    (let [w (endp/vertex ctx p)
          k (endp/edge ctx p)
          bw (blossom/in-blossom ctx w)]
      (cond
        (label/unlabeled? ctx bw)
        ;; (C1) w is a free vertex;
        ;; label w with T and label its mate with S (R12).
        {:context (assign-label ctx w c/T-BLOSSOM (endp/opposite ctx p))
         :augmented false}

        (label/s-blossom? ctx bw)
        ;; (C2) w is an S-vertex (not in the same blossom);
        ;; follow back-links to discover either an
        ;; augmenting path or a new blossom.
        (let [base (scan-blossom ctx v w)]
          (if (graph/no-node? base)
            ;; Found an augmenting path; augment the
            ;; matching and end this stage.
            {:context (augment-matching ctx k)
             :augmented true}

            ;; Found a new blossom; add it to the blossom
            ;; bookkeeping and turn it into an S-blossom.
            {:context (add-blossom ctx base k)
             :augmented false}))

        (label/unlabeled? ctx w)
        ;; w is inside a T-blossom, but w itself has not
        ;; yet been reached from outside the blossom;
        ;; mark it as reached (we need this to relabel
        ;; during T-blossom expansion).
        (do (assert (label/t-blossom? ctx bw))
            {:context (-> ctx
                          (label/add-label w c/T-BLOSSOM)
                          (label/set-endp w (endp/opposite ctx p)))
             :augmented false})

        :else
        {:context ctx
         :augmented false})))

  (scan-neighbors
    [ctx v]
    (loop [neighbors (endp/vertex-endpoints ctx v)
           result {:context ctx
                   :augmented false}]
      (if (or (not (seq neighbors))
              (:augmented result))
        result
        (let [p (first neighbors)
              k (endp/edge ctx p)
              {ctx :context} result
              w (endp/vertex ctx p)
              bv (blossom/in-blossom ctx v)
              bw (blossom/in-blossom ctx w)]
          (recur (next neighbors)
                 (if (= bv bw)
                   ;; this edge is internal to a blossom; ignore it
                   result
                   (let [{kslack :kslack ctx :context} (calc-slack ctx k)]
                     (cond (dual/allowed-edge? ctx k)
                           (consider-tight-edge ctx p v)

                           (label/s-blossom? ctx bw)
                           {:context (consider-loose-edge-to-s-blossom ctx bv k kslack)
                            :augmented false}

                           (label/unlabeled? ctx w)
                           {:context (consider-loose-edge-to-free-vertex ctx w k kslack)
                            :augmented false}

                           :else
                           {:context ctx
                            :augmented false}))))))))

  (find-augmenting-path
    [ctx]
    (loop [augmented false
           ctx ctx]
      (if (and (not (queue/queue-empty? ctx))
               (not augmented))
        (let [;; Take an S vertex from the queue.
              [v ctx] [(queue/queue-peek ctx) (queue/queue-pop ctx)]
              _ (assert (label/s-blossom? ctx (blossom/in-blossom ctx v)))
              ;; Scan its neighbours
              {ctx :context augmented :augmented}
              (scan-neighbors ctx v)]
          (recur augmented ctx))
        {:context ctx
         :augmented augmented})))

  (find-parent-blossoms
    [ctx b]
    (reverse
     (loop [iblossoms [b]]
       (let [parent (blossom/parent ctx (last iblossoms))]
         (if (graph/some-node? parent)
           (recur (conj iblossoms parent))
           iblossoms)))))

  (verify-optimum
    [ctx]
    (let [max-cardinality (options/get-option ctx :max-cardinality)
          min-dual (:delta (pdual/compute-delta-1 ctx))
          min-dual-blossoms (->> (seq (:dual-var ctx))
                                 (drop (:nvertex ctx))
                                 (reduce min))
          vdual-offset (if max-cardinality
                         ;; Vertices may have negative dual
                         ;; find a constant non-negative number to add to all vertex duals.
                         (max 0 (- min-dual))
                         0)]
      (as-> [] problems
        ;; 0. all dual variables are non-negative
        (cond-> problems
          (neg? (+ min-dual vdual-offset))
          (conj {:type :invalid-dual-vars
                 :min-dual min-dual
                 :vdual-offset vdual-offset})

          (neg? min-dual-blossoms)
          (conj {:type :invalid-dual-vars
                 :min-dual-blossoms min-dual-blossoms}))

        ;; 0. all edges have non-negative slack and
        ;; 1. all matched edges have zero slack;
        (reduce
         (fn [problems [k edge]]
           (let [[i j wt] [(graph/src edge) (graph/dest edge) (graph/weight edge)]
                 s (dual/slack ctx k)
                 iblossoms (find-parent-blossoms ctx i)
                 jblossoms (find-parent-blossoms ctx j)
                 s (->> (interleave iblossoms jblossoms)
                        (partition 2)
                        (take-while #(= (first %) (second %)))
                        (map (fn [[bi _]]
                               (* 2 (dual/dual-var ctx bi))))
                        (reduce + s))

                 matei (if (endp/no-endp? (mate/mate ctx i))
                         c/NO-EDGE
                         (endp/edge ctx (mate/mate ctx i)))
                 matej (if (endp/no-endp? (mate/mate ctx j))
                         c/NO-EDGE
                         (endp/edge ctx (mate/mate ctx j)))]
             (cond-> problems
               (or (neg? s)
                   (and (= matei k)
                        (= matej k)
                        (not (zero? s))))
               (conj {:type :invalid-edge-slack
                      :edge edge
                      :weight wt
                      :mate (:mate ctx)
                      :slack s})

               (or (and (= matei k) (not= matej k))
                   (and (= matej k) (not= matei k)))
               (conj {:type :invalid-mate
                      :k k
                      :matei matei
                      :matej matej
                      :edge edge
                      :weight wt
                      :mate (:mate ctx)
                      :slack s}))))
         problems
         (map-indexed vector (:edges ctx)))

        ;; 2. all single vertices have zero dual value
        (reduce
         (fn [problems v]
           (cond-> problems
             (not (or (endp/some-endp? (mate/mate ctx v))
                      (zero? (+ (dual/dual-var ctx v) vdual-offset))))
             (conj {:type :invalid-gnodes
                    :v v
                    :mate-v (mate/mate ctx v)
                    :dual-v (dual/dual-var ctx v)
                    :vdual-offset vdual-offset})))
         problems
         (blossom/vertex-range ctx))

        ;; 3. all blossoms with positive dual value are full.
        (reduce
         (fn [problems b]
           (cond-> problems
             (and (graph/some-node? (blossom/base ctx b))
                  (pos? (dual/dual-var ctx b))
                  (or (not (odd? (count (blossom/endps ctx b))))
                      (->> (blossom/endps ctx b)
                           (keep-indexed #(when (odd? %1) %2))
                           (some (fn [p]
                                   (or (not= (mate/mate ctx (endp/vertex ctx p))
                                             (endp/opposite ctx p))
                                       (not= (mate/mate ctx (endp/opposite-vertex ctx p))
                                             p)))))))
             (conj {:type :invalid-blossom-not-full
                    :b b})))
         problems
         (blossom/blossom-range ctx)))))

  (act-on-minimum-delta
    [ctx delta-type delta-edge delta-blossom]
    (cond-> ctx
      (some #{delta-type} [2 3])
      (as-> ctx
          (let [edge (graph/edge ctx delta-edge)
                v (if (label/unlabeled? ctx (blossom/in-blossom ctx (graph/src edge)))
                    (graph/dest edge)
                    (graph/src edge))]
            (assert (label/s-blossom? ctx (blossom/in-blossom ctx v)))
            ;; Use the least-slack edge to continue the search.
            (-> ctx
                (dual/set-allow-edge delta-edge true)
                (queue/queue-push [v]))))

      (= delta-type 4)
      ;; Expand the least-z blossom.
      (expand-blossom delta-blossom false)))

  (mate-endps-to-vertices
    [ctx]
    {:post [(valid-matching? ctx %)]}
    (->> (blossom/vertex-range ctx)
         (map #(let [mate-v (mate/mate ctx %)]
                 (if (endp/some-endp? mate-v)
                   (endp/vertex ctx mate-v)
                   c/NO-NODE)))
         vec))

  (valid-matching? [ctx matching]
    (every? (fn [v]
              (let [mate-v (nth matching v)]
                (or (graph/no-node? mate-v)
                    (= v (get matching mate-v)))))
            (blossom/vertex-range ctx))))

(defn initialize-context [edges options]
  (let [{:keys [nvertex nedge max-weight] :as g} (graph/initialize edges)]
    (ctx/map->Context (merge g
                             {:mate (vec (repeat nvertex c/NO-ENDP))
                              :label (vec (repeat (* 2 nvertex) c/FREE))
                              :label-end (vec (repeat (* 2 nvertex) c/NO-ENDP))
                              :in-blossom (vec (range nvertex))
                              :blossom-parent (vec (repeat (* 2 nvertex) c/NO-NODE))
                              :blossom-childs (vec (repeat (* 2 nvertex) []))
                              :blossom-base (vec (concat (range nvertex) (repeat nvertex c/NO-NODE)))
                              :blossom-endps (vec (repeat (* 2 nvertex) []))
                              :best-edge (vec (repeat (* 2 nvertex) c/NO-EDGE))
                              :blossom-best-edges (sorted-map)
                              :unused-blossoms (vec (range nvertex (* 2 nvertex)))
                              :dual-var (vec (concat (repeat nvertex max-weight) (repeat nvertex 0)))
                              :allow-edge (vec (repeat nedge false))
                              :queue []
                              :options options}))))

(defn max-weight-matching-impl
  [edges {:keys [max-cardinality check-optimum]
          :or {max-cardinality false
               check-optimum false}
          :as opts}]
  ;;
  ;; The algorithm is taken from "Efficient Algorithms for Finding Maximum
  ;; Matching in Graphs" by Zvi Galil, ACM Computing Surveys, 1986.
  ;; It is based on the "blossom" method for finding augmenting paths and
  ;; the "primal-dual" method for finding a matching of maximum weight, both
  ;; methods invented by Jack Edmonds.
  ;;
  ;;
  ;; Vertices are numbered 0 .. (nvertex-1).
  ;; Non-trivial blossoms are numbered nvertex .. (2*nvertex-1)
  ;;
  ;; Edges are numbered 0 .. (nedge-1).
  ;; Edge endpoints are numbered 0 .. (2*nedge-1), such that endpoints
  ;; (2*k) and (2*k+1) both belong to edge k.
  ;;
  ;;
  ;; Many terms used in the code comments are explained in the paper
  ;; by Galil. You will probably need the paper to make sense of this code.
  ;;

  (if (zero? (count edges))
    {:result []} ; don't bother with empty graphs
    (let [ctx (initialize-context edges opts)]
      ;; Main loop: continue until no further improvement is possible.
      (loop [ctx ctx]
        ;; Each iteration of this loop is a "stage".
        ;; A stage finds an augmenting path and uses that to improve
        ;; the matching.
        (let [ctx (initialize-stage ctx)

              ;; Loop until we succeed in augmenting the matching.
              [ctx augmented]
              (loop [ctx ctx]

                ;; Each iteration of this loop is a "substage".
                ;; A substage tries to find an augmenting path;
                ;; if found, the path is used to improve the matching and
                ;; the stage ends. If there is no augmenting path, the
                ;; primal-dual method is used to pump some slack out of
                ;; the dual variables.

                ;; Continue labeling until all vertices which are reachable
                ;; through an alternating path have got a label.
                (let [{ctx :context augmented :augmented} (find-augmenting-path ctx)]
                  (if augmented
                    [ctx augmented]
                    ;; There is no augmenting path under these constraints;
                    ;; compute delta and reduce slack in the optimization problem.
                    ;; (Van Rantwijk, mwmatching.py, line 732)
                    (let [{ctx :context :keys [delta-type delta-edge delta-blossom]}
                          (pdual/compute-delta ctx)
                          optimum (= delta-type 1)]
                      (if optimum
                        [ctx augmented]
                        ;; Take action at the point where minimum delta occurred.
                        (recur (act-on-minimum-delta ctx delta-type delta-edge delta-blossom)))))))]

          (if-not augmented
            ;; Stop when no more augmenting path can be found.
            (cond-> {:result (mate-endps-to-vertices ctx)
                     :context ctx}
              check-optimum
              (assoc :verify (verify-optimum ctx)))
            ;; End of a stage; expand all S-blossoms which have zero dual.
            (recur (expand-tight-sblossoms ctx))))))))

(defn max-weight-matching
  "Compute a maximum-weighted matching of G.

  A matching is a subset of edges in which no node occurs more than once.
  The weight of a matching is the sum of the weights of its edges.
  A maximal matching cannot add more edges and still be a matching.
  The cardinality of a matching is the number of matched edges.

  Parameters
  ----------
  `edges` : Edges of an undirected graph. A sequence of tuples [i j wt]
  describing an undirected edge between vertex i and vertex j with weight wt.
  There is at most one edge between any two vertices; no vertex has an edge to itself.
    Vertices are identified by consecutive, non-negative integers.
  `opts` : option map

  Options
  ----------
  max-cardinality: boolean, optional (default=false)
     If max-cardinality is true, compute the maximum-cardinality matching
     with maximum weight among all maximum-cardinality matchings.
  check-optimum: boolean, optional (default=false)
     Check optimality of solution before returning; only works on integer weights.

  Returns
  -------
  matching : collection
  A maximal matching of the graph in the form of a collection of unique vertices
  pairs.

  Notes
  -----
  This function takes time O(number_of_nodes ** 3).

  If all edge weights are integers, the algorithm uses only integer
  computations.  If floating point weights are used, the algorithm
  could return a slightly suboptimal matching due to numeric
  precision errors.

  This method is based on the \"blossom\" method for finding augmenting
  paths and the \"primal-dual\"  method for finding a matching of maximum
  weight, both methods invented by Jack Edmonds [1]_.

  References
  ----------
  .. [1] \"Efficient Algorithms for Finding Maximum Matching in Graphs\",
     Zvi Galil, ACM Computing Surveys, 1986."
  ([edges opts]
   (let [{:keys [result verify]} (max-weight-matching-impl edges opts)]
     (if-not (empty? verify)
       (throw (ex-info "Invalid optimum" {:problems verify}))
       (->> result
            (map-indexed #(when-not (= c/NO-NODE %2) #{%1 %2}))
            (filter some?)
            (into (hash-set))))))
  ([edges]
   (max-weight-matching edges {:max-cardinality false})))
