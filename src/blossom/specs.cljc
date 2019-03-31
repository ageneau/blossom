(ns blossom.specs
  (:require [clojure.spec.alpha :as s]
            [blossom.constants :as c]
            [loom.graph :refer [Graph WeightedGraph]]))

(s/def :graph/vertex nat-int?)
(s/def :graph/endpoint nat-int?)

(s/def :edge/weight (s/or :int int?
                          :number number?))

(s/def :graph/edge-index int?)
(s/def :graph/edge (s/tuple :graph/vertex :graph/vertex :edge/weight))
(s/def :context/edges (s/coll-of :graph/edge :distinct true))

(s/def :graph/graph (partial satisfies? Graph))
(s/def :graph/weighted-graph (s/and :graph/graph (partial satisfies? WeightedGraph)))

(s/def :blossom/matching (s/coll-of (s/coll-of :graph/vertex :count 2 :distinct true) :distinct true))

(s/def :node/label (s/or :unlabeled #{0}
                         :s-blossom #{1}
                         :t-blossom #{2}
                         :breadcrumb #{5}))

(s/def :node/blossom (s/or :vertex :graph/vertex
                           :no-node #{c/NO-NODE}))

(s/def :node/augmented boolean?)

;; If p is an edge endpoint,
;; endpoint[p] is the vertex to which endpoint p is attached.
;; Not modified by the algorithm.
(s/def :context/endpoint (s/coll-of :graph/vertex))

;; If v is a vertex,
;; neighbend[v] is the list of remote endpoints of the edges attached to v.
;; Not modified by the algorithm.
(s/def :context/neighbend (s/coll-of (s/coll-of :graph/endpoint)))

;; Whether all weights in the graph are integers
;; Not modified by the algorithm.
(s/def :context/integer-weights? boolean?)

;; If v is a vertex,
;; mate[v] is the remote endpoint of its matched edge, or NO-ENDP if it is single
;; (i.e. endpoint[mate[v]] is v's partner vertex).
;; Initially all vertices are single; updated during augmentation.
(s/def :context/mate (s/coll-of (s/or :remote-endpoint :graph/endpoint
                                      :single #{c/NO-ENDP})))

;; If b is a top-level blossom,
;; label.get(b) is 0 if b is unlabeled (free),
;;                 1 if b is an S-blossom,
;;                 2 if b is a T-blossom.
;; The label of a vertex is found by looking at the label of its top-level
;; containing blossom.
;; If v is a vertex inside a T-blossom, label[v] is 2 iff v is reachable
;; from an S-vertex outside the blossom.
;; Labels are assigned during a stage and reset after each augmentation.
(s/def :context/label (s/coll-of :node/label))

;; If b is a labeled top-level blossom,
;; labelend[b] is the remote endpoint of the edge through which b obtained
;; its label, or NO-ENDP if b's base vertex is single.
;; If v is a vertex inside a T-blossom and label[v] == 2,
;; labelend[v] is the remote endpoint of the edge through which v is
;; reachable from outside the blossom.
(s/def :context/label-end (s/coll-of (s/or :vertex :graph/endpoint
                                           :single #{c/NO-ENDP})))


;; If v is a vertex, in-blossom[v] is the top-level blossom to which v
;; belongs.
;; If v is a top-level vertex, in-blossom[v] == v since v is itself
;; a (trivial) top-level blossom.
;; Initially all vertices are top-level trivial blossoms.
(s/def :context/in-blossom (s/coll-of :node/blossom))

;; If b is a sub-blossom,
;; blossomparent[b] is its immediate parent (sub-)blossom.
;; If b is a top-level blossom, blossomparent[b] is NO-NODE.
(s/def :context/blossom-parent (s/coll-of :node/blossom))

;; If b is a (sub-)blossom,
;; blossombase[b] is its base VERTEX (i.e. recursive sub-blossom).
(s/def :context/blossom-base (s/coll-of :node/blossom))

;; If b is a non-trivial (sub-)blossom,
;; blossomchilds[b] is an ordered list of its sub-blossoms, starting with
;; the base and going round the blossom.
(s/def :context/blossom-childs (s/coll-of (s/coll-of :node/blossom)))

;; If b is a non-trivial (sub-)blossom,
;; blossomendps[b] is a list of endpoints on its connecting edges,
;; such that blossomendps[b][i] is the local endpoint of blossomchilds[b][i]
;; on the edge that connects it to blossomchilds[b][wrap(i+1)].
(s/def :context/blossom-endps (s/coll-of (s/coll-of :graph/vertex)))

;; If v is a free vertex (or an unreached vertex inside a T-blossom),
;; bestedge[v] is the edge to an S-vertex with least slack,
;; or NO-EDGE if there is no such edge.
;; If b is a (possibly trivial) top-level S-blossom,
;; bestedge[b] is the least-slack edge to a different S-blossom,
;; or NO-EDGE if there is no such edge.
;; This is used for efficient computation of delta2 and delta3.
(s/def :context/best-edge (s/coll-of :graph/edge-index))

;; If b is a non-trivial top-level S-blossom,
;; blossombestedges[b] is a list of least-slack edges to neighboring
;; S-blossoms, or None if no such list has been computed yet.
;; This is used for efficient computation of delta3.
(s/def :context/blossom-best-edges (s/map-of :node/blossom (s/coll-of :graph/edge-index)))

;; List of currently unused blossom numbers.
(s/def :context/unused-blossoms (s/coll-of :node/blossom))

;; If v is a vertex,
;; dualvar[v] = 2 * u(v) where u(v) is the v's variable in the dual
;; optimization problem (multiplication by two ensures integer values
;; throughout the algorithm if all edge weights are integers).
;; If b is a non-trivial blossom,
;; dualvar[b] = z(b) where z(b) is b's variable in the dual optimization
;; problem.
(s/def :context/dual-var (s/coll-of :edge/weight))

;; If allowedge[k] is true, edge k has zero slack in the optimization
;; problem; if allowedge[k] is false, the edge's slack may or may not
;; be zero.
(s/def :context/allow-edge (s/coll-of boolean?))

;; Queue of newly discovered S-vertices.
(s/def :context/queue (s/coll-of :graph/vertex))

;; FIXME comments
(s/def :context/options (s/map-of keyword? any?))

(s/def :context/nedge nat-int?)

(s/def :context/nvertex nat-int?)

(s/def :context/context (s/keys :req-un [:context/edges
                                         :context/nedge
                                         :context/nvertex
                                         :context/endpoint
                                         :context/neighbend
                                         :context/integer-weights?
                                         :context/mate
                                         :context/label
                                         :context/label-end
                                         :context/in-blossom
                                         :context/blossom-parent
                                         :context/blossom-childs
                                         :context/blossom-base
                                         :context/blossom-endps
                                         :context/best-edge
                                         :context/blossom-best-edges
                                         :context/unused-blossoms
                                         :context/dual-var
                                         :context/allow-edge
                                         :context/queue
                                         :context/options]))

;; blossom.blossom NS
(s/fdef blossom.blossom/leaves
  :args (s/cat :context :context/context
               :node :node/blossom)
  :ret (s/coll-of :graph/vertex))

(s/fdef blossom.blossom/rotate-childs
  :args (s/cat :context :context/context
               :b :node/blossom
               :i int?)
  :ret :context/context)

(s/fdef blossom.blossom/trivial-blossom?
  :args (s/cat :context :context/context
               :b :node/blossom)
  :ret boolean?)

(s/fdef blossom.blossom/set-base
  :args (s/cat :context :context/context
               :b :node/blossom
               :v :graph/vertex)
  :ret :context/context)

(s/fdef blossom.blossom/base
  :args (s/cat :context :context/context
               :b :node/blossom)
  :ret :graph/vertex)

(s/fdef blossom.blossom/set-in-blossom
  :args (s/cat :context :context/context
               :v :graph/vertex
               :b :node/blossom)
  :ret :context/context)

(s/fdef blossom.blossom/in-blossom
  :args (s/cat :context :context/context
               :v :graph/vertex)
  :ret :node/blossom)

(s/fdef blossom.blossom/set-parent
  :args (s/cat :context :context/context
               :r1 :node/blossom
               :r2 (s/nilable :node/blossom))
  :ret :context/context)

(s/fdef blossom.blossom/parent
  :args (s/cat :context :context/context
               :b :node/blossom)
  :ret (s/nilable :node/blossom))

;; blossom.label NS
(s/fdef blossom.label/add-label
  :args (s/cat :context :context/context
               :b :node/blossom
               :t :node/label)
  :ret :context/context)

(s/fdef blossom.label/label
  :args (s/cat :context :context/context
               :b :node/blossom)
  :ret :node/label)

(s/fdef blossom.label/set-endp
  :args (s/cat :context :context/context
               :b :node/blossom
               :endpoint (s/nilable :graph/vertex))
  :ret :context/context)

;; blossom.dual NS
(s/fdef blossom.dual/best-edge
  :args (s/cat :context :context/context
               :b :node/blossom)
  :ret (s/nilable :edge/edge))

(s/fdef blossom.dual/set-best-edge
  :args (s/cat :context :context/context
               :b :node/blossom
               :edge (s/nilable :edge/edge))
  :ret :context/context)

(s/fdef blossom.dual/allowed-edge?
  :args (s/cat :context :context/context
               :e :edge/edge)
  :ret boolean?)

(s/fdef blossom.dual/set-allow-edge
  :args (s/cat :context :context/context
               :edge :edge/edge)
  :ret :context/context)

(s/fdef blossom.dual/set-dual-var
  :args (s/cat :context :context/context
               :v :graph/vertex
               :x :edge/weight)
  :ret :context/context)

(s/fdef blossom.dual/dual-var
  :args (s/cat :context :context/context
               :v :graph/vertex)
  :ret :edge/weight)

(s/fdef blossom.dual/slack
  :args (s/or :vertices (s/cat :context :context/context
                               :v :graph/vertex
                               :w :graph/vertex)
              :edge (s/cat :context :context/context
                           :edge :edge/edge))
  :ret :edge/weight)

;; blossom.mate NS
(s/fdef blossom.mate/set-mate
  :args (s/cat :context :context/context
               :v1 :graph/vertex
               :v2 :graph/vertex)
  :ret :context/context)

(s/fdef blossom.mate/mate
  :args (s/cat :context :context/context
               :v :graph/vertex)
  :ret :graph/vertex)

;; blossom.queue NS
(s/fdef blossom.queue/queue-push
  :args (s/cat :context :context/context
               :coll (s/coll-of :graph/vertex :distinct true))
  :ret :context/context)

;; ns: blossom.max-weight-matching
(s/fdef blossom.max-weight-matching/initialize-context
  :args (s/cat :edges :context/edges
               :options :context/options)
  :ret :context/context)

(s/fdef blossom.max-weight-matching/entry-child-index
  :args (s/cat :context :context/context
               :b :node/blossom)
  :ret int?)

(s/fdef blossom.max-weight-matching/assign-label
  :args (s/cat :context :context/context
               :w :graph/vertex
               :t :node/label
               :p (s/or :endp :graph/endpoint
                        :no-endp #{c/NO-ENDP}))
  :ret :context/context)

(s/fdef blossom.max-weight-matching/scan-blossom
  :args (s/cat :context :context/context
               :w :graph/vertex
               :v :graph/vertex)
  :ret :context/context)

(s/fdef blossom.max-weight-matching/add-blossom
  :args (s/cat :context :context/context
               :base :graph/vertex
               :v :graph/vertex
               :w :graph/vertex)
  :ret :context/context)

(s/fdef blossom.max-weight-matching/promote-sub-blossoms-to-top-blossoms
  :args (s/cat :context :context/context
               :b :node/blossom
               :endstage boolean?)
  :ret :context/context)

(s/fdef blossom.max-weight-matching/move-to-base-relabeling
  :args (s/cat :context :context/context
               :b :node/blossom)
  :ret :context/context)

(s/fdef blossom.max-weight-matching/move-back-to-entry-child-relabeling
  :args (s/cat :context :context/context
               :b :node/blossom)
  :ret :context/context)

(s/fdef blossom.max-weight-matching/relabel-base-t-subblossom
  :args (s/cat :context :context/context
               :b :node/blossom)
  :ret :context/context)

(s/fdef blossom.max-weight-matching/expand-blossom
  :args (s/cat :context :context/context
               :b :node/blossom
               :endstage boolean?)
  :ret :context/context)

(s/fdef blossom.max-weight-matching/augment-blossom
  :args (s/cat :context :context/context
               :b :node/blossom
               :v :graph/vertex)
  :ret :context/context)

(s/fdef blossom.max-weight-matching/initialize-stage
  :args (s/cat :context :context/context)
  :ret :context/context)

(s/fdef blossom.max-weight-matching/expand-tight-sblossoms
  :args (s/cat :context :context/context)
  :ret :context/context)

(s/fdef blossom.max-weight-matching/scan-neighbors
  :args (s/cat :context :context/context
               :v :graph/vertex)
  :ret (s/keys :req-un [:context/context
                        :node/augmented]))

(s/fdef blossom.matching/maximal-matching
  :args (s/cat :g :graph/graph)
  :ret :blossom/matching)

(s/fdef blossom.matching/is-matching?
  :args (s/cat :g :graph/graph
               :matching :blossom/matching)
  :ret boolean?)

(s/fdef blossom.matching/is-perfect-matching?
  :args (s/cat :g :graph/graph
               :matching :blossom/matching)
  :ret boolean?)

(s/fdef blossom.matching/is-maximal-matching?
  :args (s/cat :g :graph/graph
               :matching :blossom/matching)
  :ret boolean?)

(s/fdef blossom.max-weight-matching/max-weight-matching
  :args (s/cat :edges :context/edges
               :opts (s/? map))
  :ret :blossom/matching)

;; ns: blossom.primal-dual
(s/fdef blossom.primal-dual/update-dual-var-with-delta
  :args (s/cat :context :context/context
               :delta :edge/weight)
  :ret :context/context)

(s/fdef blossom.primal-dual/update-blossom-dual-with-delta
  :args (s/cat :context :context/context
               :delta :edge/weight)
  :ret :context/context)

(s/fdef blossom.primal-dual/compute-delta
  :args (s/cat :context :context/context)
  :ret (s/keys :req-un [:context/context]))
