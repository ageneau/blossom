(ns blossom.blossom
  (:require [blossom.context :as ctx]
            [blossom.graph :as graph]
            [blossom.utils :as utils]))

(defprotocol IBlossom
  (in-blossom-assoc [this v b])
  (in-blossom [this v])

  (blossom-parent-assoc [this b1 b2])
  (blossom-parent-clear [this b])
  (blossom-parent [this b])

  (blossom-childs [this b])
  (blossom-childs-find [this b t])
  (blossom-childs-count [this b])
  (blossom-childs-assoc [this b childs])
  (blossom-childs-clear [this b])

  (blossom-base-assoc [this b v])
  (blossom-base-clear [this b])
  (blossom-base [this b])

  (blossom-endps-assoc [this b node-list])
  (blossom-endps-clear [this b])
  (blossom-endps [this b])

  (unused-blossoms-add [this b])
  (unused-blossoms-peek [this])
  (unused-blossoms-pop [this])

  (trivial-blossom? [this b])
  (blossom-leaves [this b])
  (blossom-rotate-childs [this b i]
    "Rotate the list of sub-blossoms to put the new base at the front.")
  (blossom-range [this])
  (vertex-range [this]))

(extend-type blossom.context.Context
  IBlossom
  (in-blossom-assoc [this v b]
    (update this :in-blossom assoc v b))

  (in-blossom [this v]
    (nth (:in-blossom this) v))

  (blossom-parent-assoc [this b1 b2]
    (update this :blossom-parent assoc b1 b2))

  (blossom-parent-clear [this b]
    (blossom-parent-assoc this b graph/NO-NODE))

  (blossom-parent [this b]
    (nth (:blossom-parent this) b))

  (blossom-childs [this b]
    (nth (:blossom-childs this) b))

  (blossom-childs-find [this b t]
    (first (utils/positions #{t} (blossom-childs this b))))

  (blossom-childs-count [this b]
    (count (blossom-childs this b)))

  (blossom-childs-assoc [this b childs]
    (update this :blossom-childs assoc b (vec childs)))

  (blossom-childs-clear [this b]
    (update this :blossom-childs update b empty))

  (blossom-base-assoc [this b v]
    (update this :blossom-base assoc b v))

  (blossom-base-clear [this b]
    (blossom-base-assoc this b graph/NO-NODE))

  (blossom-base [this b]
    (nth (:blossom-base this) b))

  (blossom-endps-assoc [this b node-list]
    (update this :blossom-endps assoc b (vec node-list)))

  (blossom-endps-clear [this b]
    (update this :blossom-endps update b empty))

  (blossom-endps [this b]
    (nth (:blossom-endps this) b))

  (unused-blossoms-add [this b]
    (update this :unused-blossoms conj b))

  (unused-blossoms-peek [this]
    (peek (:unused-blossoms this)))

  (unused-blossoms-pop [this]
    (update this :unused-blossoms pop))

  (trivial-blossom? [this b]
    (< b (:nvertex this)))

  (blossom-leaves
    [this b]
    (if-not (trivial-blossom? this b)
      (flatten (for [child (blossom-childs this b)]
                 (if-not (trivial-blossom? this child)
                   (blossom-leaves this child)
                   child)))
      [b]))

  (blossom-rotate-childs
    [this b i]
    (let [childs (utils/split-and-reverse (blossom-childs this b) i)
          endps (utils/split-and-reverse (blossom-endps this b) i)]
      (-> this
          (blossom-childs-assoc b childs)
          (blossom-endps-assoc b endps)
          (blossom-base-assoc b (blossom-base this (first childs))))))

  (blossom-range [this]
    (range (:nvertex this) (* 2 (:nvertex this))))

  (vertex-range [this]
    (range (:nvertex this))))
