(ns blossom.max-weight-matching
  (:require [blossom.blossom :as blossom]
            [blossom.context :as ctx]
            [blossom.dual :as dual]
            [blossom.graph :as graph]
            [blossom.label :as label]
            [blossom.mate :as mate]
            [blossom.options :as options]
            [blossom.queue :as queue]
            [blossom.primal-dual :as pdual]
            [blossom.utils :as utils]
            [clojure.spec.alpha :as s]))

(defprotocol IMaxWeightMatchingImpl
  (blossom-loop-direction [context b entry-child])
  (act-on-minimum-delta [context delta-type delta-edge delta-blossom])
  (promote-sub-blossoms-to-top-blossoms [context b endstage])
  (recycle-blossom [context b])
  (scan-blossom [context v w]
    "Trace back from vertices `v` and `w` to discover either a new blossom
  or an augmenting path. Return the base vertex of the new blossom,
  or NoNode if an augmenting path was found.")
  (find-parent-blossoms [context b])
  (trace-to-base [context v bb])
  (expand-tight-sblossoms [context])
  (augment-blossom [context b v]
    "Swap matched/unmatched edges over an alternating path through blossom `b`
  between vertex `v` and the base vertex. Keep blossom bookkeeping
  consistent.")
  (find-augmenting-path [context])
  (immediate-subblossom-of [context v b]
    "Starting from a vertex `v`, ascend the blossom tree, and
  return the sub-blossom immediately below `b`.")
  (expand-blossom [context b endstage]
    "Expand the given top-level blossom.
  Returns an updated `context`.")
  (augment-matching [context k]
    "Swap matched/unmatched edges over an alternating path between two
  single vertices. The augmenting path runs through S-vertices `v` and `w`.
  Returns an updated `context`.")
  (calc-slack [context k]
    "Returns a map with keys kslack and context.
  kslack is the slack for edge k context is and context is an updated context
  with a modified allow-edge cache.")
  (move-to-base-relabeling [context b])
  (initialize-stage [context])
  (augment-blossom-step [context b j x])
  (match-endpoint [context p]
    "Add endpoint p's edge to the matching.")
  (assign-label [context w t p]
    "Assign label `t` to the top-level blossom containing vertex `w`,
  and record the fact that w was reached through the edge with
  remote enpoint `p`.
  Returns an updated `context`.")
  (verify-optimum [context]
    "Verify that the optimum solution has been reached.")
  (relabel-base-t-subblossom [context b p])
  (add-blossom [context base k]
    "Construct a new blossom with given `base`, containing edge k which
  connects a pair of S vertices. Label the new blossom as S; set its dual
  variable to zero; relabel its T-vertices to S and add them to the queue.
  Returns an updated `context`.")
  (move-back-to-entry-child-relabeling [context b])
  (scan-neighbors [context v])
  (entry-child [context b])
  (first-labeled-blossom-leaf [context bv])
  (consider-loose-edge-to-free-vertex
    [context w k kslack]
    "w is a free vertex (or an unreached vertex inside
  a T-blossom) but we can not reach it yet;
  keep track of the least-slack edge that reaches w.")
  (consider-loose-edge-to-s-blossom
    [context bv k kslack]
    "keep track of the least-slack non-allowable edge to
  a different S-blossom.")
  (consider-tight-edge
    [context p v]))

(extend-type blossom.context.Context
  IMaxWeightMatchingImpl
  (entry-child [context b]
    (blossom/in-blossom context
                        (graph/endpoint context
                                        (bit-xor (label/label-end context b) 1))))

  (assign-label
    [context w t p]
    (let [b (blossom/in-blossom context w)
          base (blossom/blossom-base context b)]
      (assert (and (label/unlabeled? context w)
                   (label/unlabeled? context b)))
      (-> context
          (label/add-label w t)
          (label/add-label b t)
          (label/label-end-assoc w p)
          (label/label-end-assoc b p)
          (dual/best-edge-clear w)
          (dual/best-edge-clear b)
          (cond->
            (= label/S-BLOSSOM t)
            (queue/queue-push (if-not (blossom/trivial-blossom? context b)
                                (blossom/blossom-leaves context b)
                                [b]))

            (= label/T-BLOSSOM t)
            (assign-label (graph/endpoint context (mate/mate context base))
                          label/S-BLOSSOM
                          (bit-xor (mate/mate context base) 1))))))

  (scan-blossom
    [context v w]
    ;; Trace back from v and w, placing breadcrumbs as we go.
    (loop [path []
           base graph/NO-NODE
           v v
           w w
           context context]
      (if (and (graph/no-node? v)
               (graph/no-node? w))
        base
        (let [b (blossom/in-blossom context v)]
          ;; Look for a breadcrumb in v's blossom or put a new breadcrumb.
          (if (label/labeled-breadcrumb? context b)
            (blossom/blossom-base context b)
            (let [_ (assert (label/labeled-s-blossom? context b))
                  path (conj path b)
                  context (label/add-label context b label/BREADCRUMB)
                  ;; Trace one step back.
                  _ (assert (= (label/label-end context b)
                               (mate/mate context (blossom/blossom-base context b))))
                  v (if (graph/no-node? (label/label-end context b))
                      ;; The base of blossom b is single; stop tracing this path.
                      graph/NO-NODE
                      (let [v (graph/endpoint context (label/label-end context b))
                            b (blossom/in-blossom context v)]
                        (assert (label/labeled-t-blossom? context b))
                        ;; b is a T-blossom; trace one more step back.
                        (assert (graph/some-node? (label/label-end context b)))
                        (graph/endpoint context (label/label-end context b))))]
              ;; Swap v and w so that we alternate between both paths.
              (if (graph/some-node? w)
                (recur path base w v context)
                (recur path base v w context))))))))

  (trace-to-base
    [context v bb]
    (loop [v v
           path []]
      (let [bv (blossom/in-blossom context v)]
        (if (= bv bb)
          path
          (do
            (assert (or (label/labeled-t-blossom? context bv)
                        (and (label/labeled-s-blossom? context bv)
                             (= (label/label-end context bv)
                                (mate/mate context (blossom/blossom-base context bv))))))
            (assert (graph/some-node? (label/label-end context bv)))
            (recur (graph/endpoint context (label/label-end context bv))
                   (conj path bv)))))))

  (add-blossom
    [context base k]
    (let [[v w wt] (graph/edges context k)
          bb (blossom/in-blossom context base)

          ;; Create a new top-level blossom.
          b (blossom/unused-blossoms-peek context)
          context (-> context
                      (blossom/unused-blossoms-pop)
                      (blossom/blossom-base-assoc b base)
                      (blossom/blossom-parent-clear b)
                      (blossom/blossom-parent-assoc bb b))

          ;; Make list of sub-blossoms and their interconnecting edge endpoints.
          ;; Trace back from v to base.
          path1 (trace-to-base context v bb)

          ;; Trace back from w to base.
          path2 (trace-to-base context w bb)

          ;; Join paths, add endpoint that connects the pair of S vertices.
          path (concat [bb] (reverse path1) path2)
          endps (concat (->> path1
                             (map (fn [bv]
                                    (label/label-end context bv)))
                             reverse)
                        [(* 2 k)]
                        (map (fn [bv]
                               (bit-xor (label/label-end context bv) 1))
                             path2))

          context (reduce (fn [context bv]
                            ;; Add bv to the new blossom.
                            (blossom/blossom-parent-assoc context bv b))
                          context
                          (concat path1 path2))

          context (-> context
                      (blossom/blossom-childs-assoc b path)
                      (blossom/blossom-endps-assoc b endps))

          ;; Set label to S.
          _ (assert (label/labeled-s-blossom? context bb))
          context (-> context
                      (label/add-label b label/S-BLOSSOM)
                      (label/label-end-assoc b (label/label-end context bb))
                      ;; Set dual variable to zero.
                      (dual/dual-var-assoc b 0))

          ;; Relabel vertices.
          context (reduce (fn [context v]
                            (cond-> context
                              (label/labeled-t-blossom? context (blossom/in-blossom context v))
                              ;; This T-vertex now turns into an S-vertex because it becomes
                              ;; part of an S-blossom; add it to the queue.
                              (queue/queue-push [v])

                              :true
                              (blossom/in-blossom-assoc v b)))
                          context
                          (blossom/blossom-leaves context b))]
      (pdual/update-best-edges context b)))

  (promote-sub-blossoms-to-top-blossoms
    [context b endstage]
    (reduce (fn [context s]
              (as-> context context
                (blossom/blossom-parent-clear context s)
                (if-not (blossom/trivial-blossom? context s)
                  (if (and endstage (zero? (dual/dual-var context s)))
                    ;; Recursively expand this sub-blossom.
                    (expand-blossom context s endstage)
                    (reduce (fn [context v]
                              (blossom/in-blossom-assoc context v s))
                            context
                            (blossom/blossom-leaves context s)))

                  (blossom/in-blossom-assoc context s s))))
            context
            (blossom/blossom-childs context b)))

  (blossom-loop-direction
    [context b entry-child]
    (let [entry-child-index (blossom/blossom-childs-find context b entry-child)]
      (if (odd? entry-child-index)
        ;; Start index is odd; go forward and wrap.
        [(- entry-child-index (blossom/blossom-childs-count context b)) 1 0]
        ;; Start index is even; go backward.
        [entry-child-index -1 1])))

  (move-to-base-relabeling
    [context b]
    (let [_ (assert (graph/some-node? (label/label-end context b)))
          entry-child (entry-child context b)
          [j jstep endptrick] (blossom-loop-direction context b entry-child)]
      ;; Move along the blossom until we get to the base.
      (loop [j j
             p (label/label-end context b)
             context context]
        (if (zero? j)
          ;; Relabel the base T-sub-blossom WITHOUT stepping through to
          ;; its mate (so don't call assignLabel).
          (relabel-base-t-subblossom context b p)
          (let [endp (utils/wget (blossom/blossom-endps context b) (- j endptrick))
                context (-> context
                            ;; Relabel the T-sub-blossom.
                            (label/remove-label (graph/endpoint context (bit-xor p 1)))
                            (label/remove-label (graph/endpoint context (bit-xor endp endptrick 1)))
                            (assign-label (graph/endpoint context (bit-xor p 1)) label/T-BLOSSOM p)
                            (dual/allow-edge-assoc (quot endp 2) true))
                ;; Step to the next S-sub-blossom and note its forward endpoint.
                j (+ j jstep)
                p (bit-xor (utils/wget (blossom/blossom-endps context b) (- j endptrick)) endptrick)
                context (dual/allow-edge-assoc context (quot p 2) true)]
            ;; Step to the next T-sub-blossom.
            (recur (+ j jstep) p context))))))

  (first-labeled-blossom-leaf
    [context bv]
    (first (filter #(label/labeled? context %)
                   (blossom/blossom-leaves context bv))))

  (move-back-to-entry-child-relabeling
    [context b]
    ;; Start at the sub-blossom through which the expanding
    ;; blossom obtained its label, and relabel sub-blossoms untili
    ;; we reach the base.
    ;; Figure out through which sub-blossom the expanding blossom
    ;; obtained its label initially.
    (let [_ (assert (graph/some-node? (label/label-end context b)))
          entry-child (entry-child context b)
          [_ jstep endptrick] (blossom-loop-direction context b entry-child)
          j jstep]
      (loop [j j
             context context]
        ;; Examine the vertices of the sub-blossom to see whether
        ;; it is reachable from a neighbouring S-vertex outside the
        ;; expanding blossom.
        (let [bv (utils/wget (blossom/blossom-childs context b) j)
              v (first-labeled-blossom-leaf context bv)]
          (if (= bv entry-child)
            context
            (recur (+ j jstep)
                   (if-not (and (not (label/labeled-s-blossom? context bv))
                                (some? v))
                     context
                     (do
                       (assert (label/labeled-t-blossom? context v))
                       (assert (= (blossom/in-blossom context v) bv))
                       (-> context
                           (label/remove-label v)
                           (label/remove-label (graph/endpoint context (mate/mate context (blossom/blossom-base context bv))))
                           (assign-label v label/T-BLOSSOM (label/label-end context v)))))))))))

  (relabel-base-t-subblossom [context b p]
    (let [bv (first (blossom/blossom-childs context b))]
      (-> context
          (label/add-label bv label/T-BLOSSOM)
          (label/add-label (graph/endpoint context (bit-xor p 1)) label/T-BLOSSOM)
          (label/label-end-assoc bv p)
          (label/label-end-assoc (graph/endpoint context (bit-xor p 1)) p)
          (dual/best-edge-clear bv))))

  (recycle-blossom [context b]
    ;; Recycle the blossom number.
    (-> context
        (label/remove-label b)
        (label/label-end-clear b)
        (blossom/blossom-childs-clear b)
        (blossom/blossom-endps-clear b)
        (blossom/blossom-base-clear b)
        (dual/blossom-best-edges-clear b)
        (dual/best-edge-clear b)
        (blossom/unused-blossoms-add b)))

  (expand-blossom
    [context b endstage]
    (-> context
        (promote-sub-blossoms-to-top-blossoms b endstage)
        ;; If we expand a T-blossom during a stage, its sub-blossoms must be
        ;; relabeled.
        (cond->
            (and (not endstage)
                 (label/labeled-t-blossom? context b))
          ;; Start at the sub-blossom through which the expanding
          ;; blossom obtained its label, and relabel sub-blossoms until
          ;; we reach the base.
          ;; Figure out through which sub-blossom the expanding blossom
          ;; obtained its label initially.
          (-> (move-to-base-relabeling b) ; Move along the blossom until we get to the base.
              
              ;; Continue along the blossom until we get back to entrychild.
              (move-back-to-entry-child-relabeling b)))

        (recycle-blossom b)))

  (immediate-subblossom-of
    [context v b]
    (loop [t v]
      ;; Bubble up through the blossom tree from vertex v to an immediate
      ;; sub-blossom of b.
      (let [parent (blossom/blossom-parent context t)]
        (if-not (= b parent)
          (recur parent)
          t))))

  (augment-blossom-step [context b j x]
    (let [t (utils/wget (blossom/blossom-childs context b) j)]
      (cond-> context
        (not (blossom/trivial-blossom? context t))
        (augment-blossom t x))))

  (match-endpoint
    [context p]
    (-> context
        (mate/mate-assoc (graph/endpoint context p)
                         (bit-xor p 1))
        (mate/mate-assoc (graph/endpoint context (bit-xor p 1))
                         p)))

  (augment-blossom
    [context b v]
    (let [t (immediate-subblossom-of context v b)
          context (cond-> context
                    (not (blossom/trivial-blossom? context t))
                    ;; Recursively deal with the first sub-blossom.
                    (augment-blossom t v))

          [j jstep endptrick] (blossom-loop-direction context b t)
          entry-child-index j

          ;; Move along the blossom until we get to the base.
          context (loop [j j
                         t t
                         context context]
                    (if (zero? j)
                      context
                      (let [;; Step to the next sub-blossom and augment it recursively.
                            j (+ j jstep)
                            p (bit-xor (utils/wget (blossom/blossom-endps context b) (- j endptrick))
                                       endptrick)
                            x (graph/endpoint context p)

                            context (augment-blossom-step context b j x)

                            x (graph/endpoint context (bit-xor p 1))
                            ;; Step to the next sub-blossom and augment it recursively.
                            j (+ j jstep)
                            context (augment-blossom-step context b j x)

                            ;; Match the edge connecting those sub-blossoms.
                            context (match-endpoint context p)]
                        (recur j t context))))
          context (blossom/blossom-rotate-childs context b entry-child-index)]
      (assert (= (blossom/blossom-base context b) v))
      context))

  (augment-matching
    [context k]
    (let [[v w wt] (graph/edges context k)]
      (loop [context context
             permuted false
             s v
             p (inc (* 2 k))]
        ;; Match vertex s to remote endpoint p. Then trace back from s
        ;; until we find a single vertex, swapping matched and unmatched
        ;; edges as we go.
        (let [bs (blossom/in-blossom context s)
              _ (assert (label/labeled-s-blossom? context bs))
              _ (assert (= (label/label-end context bs)
                           (mate/mate context (blossom/blossom-base context bs))))
              context (cond-> context
                        (not (blossom/trivial-blossom? context bs))
                        (augment-blossom bs s)

                        :true
                        ;; Update mate[s]
                        (mate/mate-assoc s p))]
          ;; Trace one step back.
          (if (graph/no-node? (label/label-end context bs))
            ;; Reached single vertex; try with [s p] = [w 2*k] or stop.
            (if-not permuted
              (recur context true w (* 2 k))
              context)
            (let [t (graph/endpoint context (label/label-end context bs))
                  bt (blossom/in-blossom context t)
                  _ (assert (label/labeled-t-blossom? context bt))
                  ;; Trace one step back.
                  s (graph/endpoint context (label/label-end context bt))
                  j (graph/endpoint context (bit-xor (label/label-end context bt) 1))

                  ;; Augment through the T-blossom from j to base.
                  _ (assert (= t (blossom/blossom-base context bt)))
                  context (cond-> context
                            (not (blossom/trivial-blossom? context bt))
                            (augment-blossom bt j)

                            :true
                            ;; Update mate[j]
                            (mate/mate-assoc j (label/label-end context bt)))]
              (recur context permuted s (bit-xor (label/label-end context bt) 1))))))))

  (initialize-stage
    [context]
    (-> context
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
        (as-> context
            (reduce (fn [context v]
                      (cond-> context
                        (and (graph/no-node? (mate/mate context v))
                             (label/unlabeled? context (blossom/in-blossom context v)))
                        (assign-label v label/S-BLOSSOM graph/NO-NODE)))
                    context
                    (blossom/vertex-range context)))))

  (expand-tight-sblossoms
    [context]
    (reduce (fn [context b]
              (cond-> context
                (and (graph/no-node? (blossom/blossom-parent context b))
                     (graph/some-node? (blossom/blossom-base context b))
                     (label/labeled-s-blossom? context b)
                     (zero? (dual/dual-var context b)))
                (expand-blossom b true)))
            context
            (blossom/blossom-range context)))

  (consider-loose-edge-to-free-vertex
    [context w k kslack]
    (cond-> context
      (or (graph/no-edge? (dual/best-edge context w))
          (< kslack
             (dual/slack context (dual/best-edge context w))))
      (dual/best-edge-assoc w k)))

  (consider-loose-edge-to-s-blossom
    [context bv k kslack]
    (cond-> context
      (or (graph/no-edge? (dual/best-edge context bv))
          (< kslack
             (dual/slack context (dual/best-edge context bv))))
      (dual/best-edge-assoc bv k)))

  (calc-slack
    [context k]
    (let [kslack (dual/slack context k)]
      {:kslack kslack
       :context (cond-> context
                  (and (not (dual/allowed-edge? context k))
                       (<= kslack 0))
                  ;; edge k has zero slack => it is allowable
                  (dual/allow-edge-assoc k true))}))

  (consider-tight-edge
    [context p v]
    (let [w (graph/endpoint context p)
          k (quot p 2)
          bw (blossom/in-blossom context w)]
      (cond
        (label/unlabeled? context bw)
        ;; (C1) w is a free vertex;
        ;; label w with T and label its mate with S (R12).
        {:context (assign-label context w label/T-BLOSSOM (bit-xor p 1))
         :augmented false}

        (label/labeled-s-blossom? context bw)
        ;; (C2) w is an S-vertex (not in the same blossom);
        ;; follow back-links to discover either an
        ;; augmenting path or a new blossom.
        (let [base (scan-blossom context v w)]
          (if (graph/no-node? base)
            ;; Found an augmenting path; augment the
            ;; matching and end this stage.
            {:context (augment-matching context k)
             :augmented true}

            ;; Found a new blossom; add it to the blossom
            ;; bookkeeping and turn it into an S-blossom.
            {:context (add-blossom context base k)
             :augmented false}))

        (label/unlabeled? context w)
        ;; w is inside a T-blossom, but w itself has not
        ;; yet been reached from outside the blossom;
        ;; mark it as reached (we need this to relabel
        ;; during T-blossom expansion).
        (do (assert (label/labeled-t-blossom? context bw))
            {:context (-> context
                          (label/add-label w label/T-BLOSSOM)
                          (label/label-end-assoc w (bit-xor p 1)))
             :augmented false})

        :else
        {:context context
         :augmented false})))

  (scan-neighbors
    [context v]
    (loop [neighbors (graph/successors* context v)
           result {:context context
                   :augmented false}]
      (if (or (not (seq neighbors))
              (:augmented result))
        result
        (let [p (first neighbors)
              k (quot p 2)
              {:keys [context augmented]} result
              w (graph/endpoint context p)
              bv (blossom/in-blossom context v)
              bw (blossom/in-blossom context w)]
          (recur (next neighbors)
                 (if (= bv bw)
                   ;; this edge is internal to a blossom; ignore it
                   result
                   (let [{:keys [kslack context]} (calc-slack context k)]
                     (cond (dual/allowed-edge? context k)
                           (consider-tight-edge context p v)

                           (label/labeled-s-blossom? context bw)
                           {:context (consider-loose-edge-to-s-blossom context bv k kslack)
                            :augmented false}

                           (label/unlabeled? context w)
                           {:context (consider-loose-edge-to-free-vertex context w k kslack)
                            :augmented false}

                           :else
                           {:context context
                            :augmented false}))))))))

  (find-augmenting-path
    [context]
    (loop [augmented false
           context context]
      (if (and (not (queue/queue-empty? context))
               (not augmented))
        (let [;; Take an S vertex from the queue.
              [v context] [(queue/queue-peek context) (queue/queue-pop context)]
              _ (assert (label/labeled-s-blossom? context (blossom/in-blossom context v)))
              ;; Scan its neighbours
              {:keys [context augmented]}
              (scan-neighbors context v)]
          (recur augmented context))
        {:context context
         :augmented augmented})))

  (find-parent-blossoms
    [context b]
    (reverse
     (loop [iblossoms [b]]
       (let [parent (blossom/blossom-parent context (last iblossoms))]
         (if (graph/some-node? parent)
           (recur (conj iblossoms parent))
           iblossoms)))))

  (verify-optimum
    [context]
    (let [max-cardinality (options/get-option context :max-cardinality)
          min-dual (:delta (pdual/compute-delta-1 context))
          min-dual-blossoms (reduce min (subvec (:dual-var context) (:nvertex context)))
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
         (fn [problems [k [i j wt :as edge]]]
           (let [s (dual/slack context k)
                 iblossoms (find-parent-blossoms context i)
                 jblossoms (find-parent-blossoms context j)
                 s (->> (interleave iblossoms jblossoms)
                        (partition 2)
                        (take-while #(= (first %) (second %)))
                        (map (fn [[bi bj]]
                               (* 2 (dual/dual-var context bi))))
                        (reduce + s))

                 matei (if (graph/no-node? (mate/mate context i))
                         graph/NO-NODE
                         (quot (mate/mate context i) 2))
                 matej (if (graph/no-node? (mate/mate context j))
                         graph/NO-NODE
                         (quot (mate/mate context j) 2))]
             (cond-> problems
               (or (neg? s)
                   (and (= matei k)
                        (= matej k)
                        (not (zero? s))))
               (conj {:type :invalid-edge-slack
                      :edge edge
                      :weight wt
                      :mate (:mate context)
                      :slack s})

               (or (and (= matei k) (not= matej k))
                   (and (= matej k) (not= matei k)))
               (conj {:type :invalid-mate
                      :k k
                      :matei matei
                      :matej matej
                      :edge edge
                      :weight wt
                      :mate (:mate context)
                      :slack s}))))
         problems
         (map-indexed vector (:edges context)))

        ;; 2. all single vertices have zero dual value
        (reduce
         (fn [problems v]
           (cond-> problems
             (not (or (graph/some-node? (mate/mate context v))
                      (zero? (+ (dual/dual-var context v) vdual-offset))))
             (conj {:type :invalid-gnodes
                    :v v
                    :mate-v (mate/mate context v)
                    :dual-v (dual/dual-var context v)
                    :vdual-offset vdual-offset})))
         problems
         (blossom/vertex-range context))

        ;; 3. all blossoms with positive dual value are full.
        (reduce
         (fn [problems b]
           (cond-> problems
             (and (graph/some-node? (blossom/blossom-base context b))
                  (pos? (dual/dual-var context b))
                  (or (not (odd? (count (blossom/blossom-endps context b))))
                      (->> (blossom/blossom-endps context b)
                           (keep-indexed #(if (odd? %1) %2))
                           (some (fn [p]
                                   (or (not= (mate/mate context
                                                        (graph/endpoint context p))
                                             (bit-xor p 1))
                                       (not= (mate/mate context
                                                        (graph/endpoint context (bit-xor p 1)))
                                             p)))))))
             (conj {:type :invalid-blossom-not-full
                    :b b})))
         problems
         (blossom/blossom-range context)))))

  (act-on-minimum-delta
    [context delta-type delta-edge delta-blossom]
    (cond-> context
      (some #{delta-type} [2 3])
      (as-> context
          (let [[v w] (-> (graph/edges context delta-edge)
                          (subvec 0 2))

                [v w] (cond-> [v w]
                        (label/unlabeled? context
                                             (blossom/in-blossom context v))
                        (-> reverse vec))]
            (assert (label/labeled-s-blossom? context
                                              (blossom/in-blossom context v)))
            ;; Use the least-slack edge to continue the search.
            (-> context
                (dual/allow-edge-assoc delta-edge true)
                (queue/queue-push [v]))))

      (= delta-type 4)
      ;; Expand the least-z blossom.
      (expand-blossom delta-blossom false))))

(defn initialize-context [edges options]
  (let [{:keys [nvertex nedge] :as g} (graph/initialize edges)
        max-weight (graph/max-weight g)]
    (ctx/map->Context (merge g
                             {:mate (vec (repeat nvertex graph/NO-NODE))
                              :label (vec (repeat (* 2 nvertex) label/FREE))
                              :label-end (vec (repeat (* 2 nvertex) graph/NO-NODE))
                              :in-blossom (vec (range nvertex))
                              :blossom-parent (vec (repeat (* 2 nvertex) graph/NO-NODE))
                              :blossom-childs (vec (repeat (* 2 nvertex) []))
                              :blossom-base (vec (concat (range nvertex) (repeat nvertex graph/NO-NODE)))
                              :blossom-endps (vec (repeat (* 2 nvertex) []))
                              :best-edge (vec (repeat (* 2 nvertex) graph/NO-EDGE))
                              :blossom-best-edges (sorted-map)
                              :unused-blossoms (vec (range nvertex (* 2 nvertex)))
                              :dual-var (vec (concat (repeat nvertex max-weight) (repeat nvertex 0)))
                              :allow-edge (vec (repeat nedge false))
                              :queue []
                              :options options}))))

(defn max-weight-matching-impl
  [edges {:keys [max-cardinality]
          :or {max-cardinality false}
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
    (let [context (initialize-context edges opts)]
      ;; Main loop: continue until no further improvement is possible.
      (loop [context context]
        ;; Each iteration of this loop is a "stage".
        ;; A stage finds an augmenting path and uses that to improve
        ;; the matching.
        (let [context (initialize-stage context)

              ;; Loop until we succeed in augmenting the matching.
              [context augmented]
              (loop [augmented false
                     context context]

                ;; Each iteration of this loop is a "substage".
                ;; A substage tries to find an augmenting path;
                ;; if found, the path is used to improve the matching and
                ;; the stage ends. If there is no augmenting path, the
                ;; primal-dual method is used to pump some slack out of
                ;; the dual variables.

                ;; Continue labeling until all vertices which are reachable
                ;; through an alternating path have got a label.
                (let [{:keys [context augmented]} (find-augmenting-path context)]
                  (if augmented
                    [context augmented]
                    ;; There is no augmenting path under these constraints;
                    ;; compute delta and reduce slack in the optimization problem.
                    ;; (Van Rantwijk, mwmatching.py, line 732)
                    (let [{:keys [context delta-type delta-edge delta-blossom]}
                          (pdual/compute-delta context)
                          optimum (= delta-type 1)]
                      (if optimum
                        [context augmented]
                        ;; Take action at the point where minimum delta occurred.
                        (recur augmented
                               (act-on-minimum-delta context delta-type delta-edge delta-blossom)))))))]

          (if-not augmented
            ;; Stop when no more augmenting path can be found.
            (let [verify (when (options/get-option context :check-optimum)
                           (verify-optimum context))
                  ;; Transform mate[] such that mate[v] is the vertex to which v is paired.
                  context (reduce (fn [context v]
                                    (let [mate-v (mate/mate context v)]
                                      (cond-> context
                                        (graph/some-node? mate-v)
                                        (mate/mate-assoc v (graph/endpoint context mate-v)))))
                                  context
                                  (blossom/vertex-range context))]

              ;; Paranoia check that the matching is symmetric.
              (doseq [v (blossom/vertex-range context)
                      :let [mate-v (mate/mate context v)]]
                (assert (or (graph/no-node? mate-v)
                            (= v (mate/mate context mate-v)))))
              {:context context
               :result (:mate context)
               :verify verify})
            ;; End of a stage; expand all S-blossoms which have zero dual.
            (recur (expand-tight-sblossoms context))))))))

(defn max-weight-matching
  "Compute a maximum-weighted matching of G.

  A matching is a subset of edges in which no node occurs more than once.
  The weight of a matching is the sum of the weights of its edges.
  A maximal matching cannot add more edges and still be a matching.
  The cardinality of a matching is the number of matched edges.

  Parameters
  ----------
  `edges` : Edges of an undirected graph. A sequence of tuples (i, j, wt)
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
      A maximal matching of the graph. Such that mate[i] == j if vertex i is
    matched to vertex j, and mate[i] == -1 if vertex i is not matched.

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
   (let [{:keys [context result verify]} (max-weight-matching-impl edges opts)]
     (if-not (empty? verify)
       (throw (ex-info "Invalid optimum" {:problems verify})))
     result))
  ([edges]
   (max-weight-matching edges {:max-cardinality false})))
