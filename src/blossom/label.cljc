(ns blossom.label
  (:require [blossom.constants :as c]
            [blossom.graph :as graph]))

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

  (label-endp-assoc [graph node endpoint])
  (label-endp-clear [graph node])
  (label-endp [graph node]))

(extend-type blossom.context.Context
  PLabelable
  (add-label [this node t]
    (update this :label assoc node t))

  (remove-label [this node]
    (add-label this node c/FREE))

  (label [this b]
    (nth (:label this) b))

  (unlabeled? [this v]
    (= c/FREE (label this v)))
  (labeled? [this v]
    (not= c/FREE (label this v)))
  (labeled-s-blossom? [this v]
    (= c/S-BLOSSOM (label this v)))
  (labeled-t-blossom? [this v]
    (= c/T-BLOSSOM (label this v)))
  (labeled-breadcrumb? [this v]
    (= c/BREADCRUMB (label this v)))

  (label-endp-assoc [this b endpoint]
    (update this :label-end assoc b endpoint))

  (label-endp-clear [this b]
    (label-endp-assoc this b c/NO-ENDP))

  (label-endp [this b]
    (nth (:label-end this) b))

  (remove-all-labels [this]
    (assoc this :label (vec (repeat (* 2 (:nvertex this)) c/FREE)))))
