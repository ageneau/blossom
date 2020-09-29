(ns blossom.networkx
  (:require [blossom.constants :as c]
            [blossom.max-weight-matching :as mwm]
            [blossom.test-utils :as utils]
            [camel-snake-kebab.core :as kebab]
            [clojure.set]
            [clojure.walk :as walk]
            [clojure.spec.alpha :as s]))

(defn adapt-boolean [in]
  ((complement zero?) in))

(defn adapt-arg-list
  ([args names]
   (vec
    (for [k names]
      (let [[adapter arg] (if (fn? (first k))
                            k
                            [identity k])]
        (adapter (get args arg)))))))

(defn adapt-vertex [in]
  {:pre [(int? in)]}
  (if (= in -1)
    c/NO-NODE
    in))

(defn adapt-edge-index [in]
  {:pre [(int? in)]}
  (if (= in -1)
    c/NO-EDGE
    in))

(defn adapt-endpoint [in]
  {:pre [(int? in)]}
  (if (= in -1)
    c/NO-ENDP
    in))

(def adapt-label identity)

(def adapt-blossom adapt-vertex)

(defn adapt-blossom-best-edges [in]
  (into (sorted-map)
        (keep-indexed (fn [idx item]
                        {:pre [(s/valid? (s/nilable (s/coll-of int?)) item)]}
                        (when item [idx (vec (map adapt-edge-index item))]))
                      in)))

(defn adapt-context [in]
  (let [in (walk/keywordize-keys in)
        edges (get in :edges)
        options {:sort-vertices true
                 :max-cardinality (get in :max-cardinality)}
        context (mwm/initialize-context edges options)]
    (assert (= (:nedge context) (get in :nedge)))
    (assert (= (:nvertex context) (get in :nvertex)))
    (assert (= (:endpoint context) (get in :endpoint)))
    (assert (= (:neighbend context) (get in :neighbend)))
    (-> context
        (merge (select-keys in
                            (clojure.set/intersection (set (keys context))
                                                      (set (keys in)))))
        (update :endpoint (comp vec (partial map adapt-vertex)))
        (update :neighbend (comp vec (partial map (comp vec (partial map adapt-endpoint)))))
        (update :mate (comp vec (partial map adapt-endpoint)))
        (update :label (comp vec (partial map adapt-label)))
        (update :label-end (comp vec (partial map adapt-endpoint)))
        (update :in-blossom (comp vec (partial map adapt-blossom)))
        (update :blossom-parent (comp vec (partial map adapt-blossom)))
        (update :blossom-childs (comp vec (partial map (comp vec (partial map adapt-vertex)))))
        (update :blossom-base (comp vec (partial map adapt-blossom)))
        (update :blossom-endps (comp vec (partial map (comp vec (partial map adapt-endpoint)))))
        (update :best-edge (comp vec (partial map adapt-edge-index)))
        (update :blossom-best-edges adapt-blossom-best-edges)
        (update :unused-blossoms (comp vec (partial map adapt-blossom)))
        (update :queue (comp vec (partial map adapt-vertex))))))

(defmulti adapt-arg-list-for-func (fn [args context-pre fname] fname))

(defmethod adapt-arg-list-for-func :default
  [args context-pre fname-kw]
  (if (empty? args)
    []
    (throw (ex-info "No args adapter" {:fname fname-kw
                                       :args args}))))

(defmethod adapt-arg-list-for-func :assign-label
  [args context-pre fname-kw]
  (adapt-arg-list args ["w" "t" [adapt-endpoint "p"]]))

(defmethod adapt-arg-list-for-func :scan-blossom
  [args context-pre fname-kw]
  (adapt-arg-list args ["v" "w"]))

(defmethod adapt-arg-list-for-func :augment-blossom
  [args context-pre fname-kw]
  (adapt-arg-list args ["b" "v"]))

(defmethod adapt-arg-list-for-func :augment-matching
  [args context-pre fname-kw]
  (adapt-arg-list args ["k"]))

(defmethod adapt-arg-list-for-func :add-blossom
  [args context-pre fname-kw]
  (adapt-arg-list args ["base" "k"]))

(defmethod adapt-arg-list-for-func :expand-blossom
  [args context-pre fname-kw]
  (adapt-arg-list args ["b" "endstage"]))

(defmethod adapt-arg-list-for-func :max-weight-matching
  [args context-pre fname-kw]
  (concat (adapt-arg-list args ["edges"])
          [{:max-cardinality (get args "maxcardinality")
            :sort-vertices true}]))

(defmethod adapt-arg-list-for-func :scan-neighbors
  [args context-pre fname-kw]
  (adapt-arg-list args ["v"]))

(defmethod adapt-arg-list-for-func :find-augmenting-path
  [args context-pre fname-kw]
  [])

(defmethod adapt-arg-list-for-func :compute-delta
  [args context-pre fname-kw]
  [])

(defmethod adapt-arg-list-for-func :expand-tight-sblossoms
  [args context-pre fname-kw]
  [])

(defmethod adapt-arg-list-for-func :promote-sub-blossoms-to-top-blossoms
  [args context-pre fname-kw]
  (adapt-arg-list args ["b" "endstage"]))

(defmethod adapt-arg-list-for-func :move-to-base-relabeling
  [args context-pre fname-kw]
  (adapt-arg-list args ["b"]))

(defmethod adapt-arg-list-for-func :move-back-to-entry-child-relabeling
  [args context-pre fname-kw]
  (adapt-arg-list args ["b"]))

(defmethod adapt-arg-list-for-func :relabel-base-t-subblossom
  [args context-pre fname-kw]
  (adapt-arg-list args ["b"]))

(defmulti adapt-ret-for-func (fn [ret context-post fname-kw] fname-kw))

(defmethod adapt-ret-for-func :default
  [ret context-post fname-kw]
  (identity ret))

(defmethod adapt-ret-for-func :scan-blossom
  [ret context-post fname-kw]
  (adapt-vertex ret))

(defmethod adapt-ret-for-func :scan-neighbors
  [ret context-post fname-kw]
  (adapt-boolean ret))

(defmethod adapt-ret-for-func :find-augmenting-path
  [ret context-post fname-kw]
  (adapt-boolean ret))

(defmethod adapt-ret-for-func :compute-delta
  [ret context-post fname-kw]
  (->> ret
       (map #(when (some? (second %))
               (vector (keyword (first %)) (second %))))
       (into {})))

(defn adapt-verify-optimum-result [in]
  (->> in
       (map walk/keywordize-keys)
       (map #(reduce-kv (fn [problem k v]
                          (cond-> (assoc problem k v)
                            (#{:type} k)
                            (update k keyword)

                            (#{:mate-v :mate :matei :matej} k)
                            (update k adapt-endpoint)

                            (#{:k} k)
                            (update k adapt-edge-index)

                            (#{:v} k)
                            (update k adapt-vertex)))
                        {}
                        %))
       vec))

(defmethod adapt-ret-for-func :verify-optimum
  [ret context-post fname-kw]
  (adapt-verify-optimum-result ret))

(defmethod adapt-ret-for-func :max-weight-matching
  [ret context-post fname-kw]
  (map adapt-vertex ret))

(defn read-log [file]
  (->> file
       utils/parse-json
       (map-indexed (fn [idx call-log]
                      (as-> call-log call-log
                        (reduce (fn [altered-map [k v]]
                                  (assoc altered-map
                                         (kebab/->kebab-case-keyword k)
                                         v))
                                {}
                                call-log)
                        (assoc call-log :log-index idx)
                        (update call-log :function kebab/->kebab-case-keyword)
                        (update call-log :type kebab/->kebab-case-keyword)
                        (update call-log :context adapt-context)
                        (cond-> call-log
                          (#{:fn-call} (:type call-log))
                          (assoc :args (adapt-arg-list-for-func (:args call-log)
                                                                (:context call-log)
                                                                (:function call-log)))

                          (#{:return-procedure} (:type call-log))
                          (assoc :procedure? true)

                          (#{:return-function} (:type call-log))
                          (assoc :procedure? false)

                          (#{:return-procedure :return-function} (:type call-log))
                          (-> (assoc :ret (adapt-ret-for-func (:ret call-log)
                                                              (:context call-log)
                                                              (:function call-log)))
                              (assoc :type :return))))))
       (group-by :id)
       (map (fn [[id call-logs]]
              [id (->> call-logs
                       (group-by :type)
                       (reduce-kv (fn [out k v]
                                    (cond-> (assoc out k v)
                                      (#{:fn-call :return} k)
                                      (update k first)))
                                  {}))]))
       (into {})))


