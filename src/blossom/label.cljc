(ns blossom.label
  (:require [blossom.graph :as graph]))

(def FREE 0)
(def S-BLOSSOM 1)
(def T-BLOSSOM 2)
(def BREADCRUMB 5)

(defprotocol PLabelable
  (add-label [graph node label]
    "Add a label to a node")
  (label [graph node]
    "Return the label on a node")
  (remove-label [graph node]
    "Remove a label from a node")
  (remove-all-labels [graph]
    "remove all labels from the graph")

  (unlabeled? [graph node])
  (labeled? [graph node])
  (labeled-s-blossom? [graph node])
  (labeled-t-blossom? [graph node])
  (labeled-breadcrumb? [graph node])

  (label-end-assoc [graph node endpoint])
  (label-end-clear [graph node])
  (label-end [graph node]))

(extend-type blossom.context.Context
  PLabelable
  (add-label [this node t]
    (update this :label assoc node t))

  (remove-label [this node]
    (add-label this node FREE))

  (label [this b]
    (nth (:label this) b))

  (unlabeled? [this v]
    (= FREE (label this v)))
  (labeled? [this v]
    (not= FREE (label this v)))
  (labeled-s-blossom? [this v]
    (= S-BLOSSOM (label this v)))
  (labeled-t-blossom? [this v]
    (= T-BLOSSOM (label this v)))
  (labeled-breadcrumb? [this v]
    (= BREADCRUMB (label this v)))

  (label-end-assoc [this b endpoint]
    (update this :label-end assoc b endpoint))

  (label-end-clear [this b]
    (label-end-assoc this b graph/NO-NODE))

  (label-end [this b]
    (nth (:label-end this) b))

  (remove-all-labels [this]
    (assoc this :label (vec (repeat (* 2 (:nvertex this)) FREE)))))
