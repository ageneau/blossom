(ns blossom.blossom
  (:require [blossom.constants :as c]
            [blossom.context :as ctx]
            [blossom.graph :as graph]
            [blossom.utils :as utils]))

(defprotocol PBlossom
  (set-in-blossom [this v b])
  (in-blossom [this v])

  (set-parent [this child parent])
  (remove-parent [this b])
  (parent [this b])

  (childs [this b])
  (child [this b n]
    "Get child for blossom b at index n, wrapping negative indices if necessary.")
  (childs-find [this b t])
  (childs-count [this b])
  (set-childs [this b childs])
  (childs-clear [this b])

  (set-base [this b v])
  (base-clear [this b])
  (base [this b])

  (set-endps [this b node-list])
  (endps-clear [this b])
  (endps [this b])
  (endpoint [this b n]
    "Get endpoint for blossom b at index n, wrapping negative indices if necessary.")

  (unused-add [this b])
  (unused-peek [this])
  (unused-pop [this])

  (trivial-blossom? [this b])
  (leaves [this b])
  (rotate-childs [this b i]
    "Rotate the list of sub-blossoms to put the new base at the front.")
  (blossom-range [this])
  (vertex-range [this]))

(extend-type blossom.context.Context
  PBlossom
  (set-in-blossom [this v b]
    (update this :in-blossom assoc v b))

  (in-blossom [this v]
    (nth (:in-blossom this) v))

  (set-parent [this child parent]
    (update this :blossom-parent assoc child parent))

  (remove-parent [this b]
    (set-parent this b c/NO-NODE))

  (parent [this b]
    (nth (:blossom-parent this) b))

  (childs [this b]
    (nth (:blossom-childs this) b))

  (child [this b n]
    (utils/wget (childs this b) n))

  (childs-find [this b t]
    (first (utils/positions #{t} (childs this b))))

  (childs-count [this b]
    (count (childs this b)))

  (set-childs [this b childs]
    (update this :blossom-childs assoc b (vec childs)))

  (childs-clear [this b]
    (update this :blossom-childs update b empty))

  (set-base [this b v]
    (update this :blossom-base assoc b v))

  (base-clear [this b]
    (set-base this b c/NO-NODE))

  (base [this b]
    (nth (:blossom-base this) b))

  (set-endps [this b node-list]
    (update this :blossom-endps assoc b (vec node-list)))

  (endps-clear [this b]
    (update this :blossom-endps update b empty))

  (endps [this b]
    (nth (:blossom-endps this) b))

  (endpoint [this b n]
    (utils/wget (endps this b) n))

  (unused-add [this b]
    (update this :unused-blossoms conj b))

  (unused-peek [this]
    (peek (:unused-blossoms this)))

  (unused-pop [this]
    (update this :unused-blossoms pop))

  (trivial-blossom? [this b]
    (< b (:nvertex this)))

  (leaves
    [this b]
    (if-not (trivial-blossom? this b)
      (flatten (for [child (childs this b)]
                 (if-not (trivial-blossom? this child)
                   (leaves this child)
                   child)))
      [b]))

  (rotate-childs
    [this b i]
    (let [childs (utils/split-and-reverse (childs this b) i)
          endps (utils/split-and-reverse (endps this b) i)]
      (-> this
          (set-childs b childs)
          (set-endps b endps)
          (set-base b (base this (first childs))))))

  (blossom-range [this]
    (range (:nvertex this) (* 2 (:nvertex this))))

  (vertex-range [this]
    (range (:nvertex this))))
