(ns blossom.label
  (:require [blossom.context]
            [blossom.constants :as c]
            [blossom.endpoint :as endpoint]))

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
  (s-blossom? [graph node])
  (t-blossom? [graph node])
  (breadcrumb? [graph node])

  (set-endp [graph node endpoint])
  (remove-endp [graph node])
  (endp [graph node])
  (some-endp? [graph node])
  (no-endp? [graph node]))

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
  (s-blossom? [this v]
    (= c/S-BLOSSOM (label this v)))
  (t-blossom? [this v]
    (= c/T-BLOSSOM (label this v)))
  (breadcrumb? [this v]
    (= c/BREADCRUMB (label this v)))

  (set-endp [this b endpoint]
    (update this :label-end assoc b endpoint))

  (remove-endp [this b]
    (set-endp this b c/NO-ENDP))

  (endp [this b]
    (nth (:label-end this) b))

  (some-endp? [graph node]
    (endpoint/some-endp? (endp graph node)))

  (no-endp? [graph node]
    (endpoint/no-endp? (endp graph node)))

  (remove-all-labels [this]
    (assoc this :label (vec (repeat (* 2 (:nvertex this)) c/FREE)))))
