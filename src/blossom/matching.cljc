(ns blossom.matching
  (:require [clojure.spec.alpha :as s]
            [blossom.endpoint :as endp]
            [loom.graph :as lg]))

(defn maximal-matching
  "Find a maximal matching in the graph.

  A matching is a subset of edges in which no node occurs more than once.
  A maximal matching cannot add more edges and still be a matching.

  Parameters
  ----------
  `g` : Undirected graph

  Returns
  -------
  matching : set
      A maximal matching of the graph.

  Notes
  -----
  The algorithm greedily selects a maximal matching M of the graph G
  (i.e. no superset of M exists). It runs in $O(|E|)$ time."
  [g]
  (as-> {:matching #{}
         :nodes #{}} result
    (reduce (fn [{:keys [matching nodes] :as result} [u v :as edge]]
              (cond-> result
                (and (not= u v)
                     (not (contains? nodes u))
                     (not (contains? nodes v)))
                (-> (update :matching conj #{u v})
                    (update :nodes conj u v))))
            result
            (lg/edges g))
    (get result :matching)))

(defn is-matching?
  "Decides whether the given set represents a valid matching in G.

  A *matching* in a graph is a set of edges in which no two distinct
  edges share a common endpoint.

  Parameters
  ----------
  `g` : graph

  `matching` : set
      A set representing a matching. It must have elements
      of the form #{u v}, where [u v] is an edge in the
      matching.

  Returns
  -------
  boolean
      Whether the given set represents a valid matching
      in the graph."
  [g matching]
  (and (s/valid? :blossom/matching matching)
       (let [{:keys [all-distinct all-edges]}
             (reduce (fn [result [u v]]
                       (cond-> result
                         (some #{u v} (:nodes result))
                         (assoc :all-distinct false)

                         (not (lg/has-edge? g u v))
                         (assoc :all-edges false)

                         :true
                         (update :nodes conj u v)))
                     {:nodes #{}
                      :all-distinct true
                      :all-edges true}
                     (map vec matching))]
         (and all-distinct all-edges))))

(defn is-perfect-matching?
  "Decides whether the given set represents a valid perfect matching in `g`

  A *perfect matching* in a graph is a matching in which exactly one edge
  is incident upon each vertex.

  Parameters
  ----------
  `g` : graph

  `matching` : set
      A set representing a matching. It must have elements
      of the form #{u v}, where [u v] is an edge in the
      matching.

   Returns
   -------
   boolean
      Whether the given set or dictionary represents a valid perfect
      matching in the graph."
  [g matching]
  (let [incident-edges-freqs (->> matching
                                  (mapcat identity)
                                  frequencies
                                  vals)]
    (and (is-matching? g matching)
         (every? #{1} incident-edges-freqs)
         (= (count (lg/nodes g))
            (count incident-edges-freqs)))))

(defn is-maximal-matching?
  "Decides whether the given set represents a valid
  maximal matching in G.

  A *maximal matching* in a graph is a matching in which adding any
  edge would cause the set to no longer be a valid matching.

  Parameters
  ----------
  `g` : graph

  `matching` : set
      A set representing a matching. It must have elements
      of the form #{u v}, where [u v] is an edge in the
      matching.

  Returns
  -------
  boolean
      Whether the given set represents a valid maximal
      matching in the graph."
  [g matching]
  (and (is-matching? g matching)
       (not-any? true?
                 (map (fn [edge]
                        (and (not (contains? matching (set edge)))
                             (is-matching? g (conj matching (set edge)))))
                      (lg/edges g)))))


(defn matching-to-set [matching]
  (reduce (fn [result [a b]]
            (cond-> result
              (and (endp/some-endp? a)
                   (endp/some-endp? b))
              (conj #{a b})))
          #{}
          (map-indexed vector matching)))
