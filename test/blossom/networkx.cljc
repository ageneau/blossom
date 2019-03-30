(ns blossom.networkx
  (:require [blossom.max-weight-matching :as mwm]
            [blossom.test-utils :as utils]
            [camel-snake-kebab.core :as kebab]
            [clojure.set]
            [clojure.walk :as walk]))

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

(defn adapt-blossom-best-edges [in]
  (into (sorted-map)
        (keep-indexed (fn [idx item]
                        (when item [idx item]))
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
        (update :blossom-best-edges adapt-blossom-best-edges)
        (update :blossom-childs #(vec (map vec %)))
        (update :blossom-endps #(vec (map vec %))))))

(defmulti adapt-arg-list-for-func (fn [args context-pre fname] fname))

(defmethod adapt-arg-list-for-func :default
  [args context-pre fname-kw]
  (if (empty? args)
    []
    (throw (ex-info "No args adapter" {:fname fname-kw
                                       :args args}))))

(defmethod adapt-arg-list-for-func :assign-label
  [args context-pre fname-kw]
  (adapt-arg-list args ["w" "t" "p"]))

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

(defmethod adapt-ret-for-func :verify-optimum
  [ret context-post fname-kw]
  (->> ret
       (map walk/keywordize-keys)
       (map (fn [problem]
              (update problem :type keyword)))
       vec))

(defn read-log [file]
  (->> file
       utils/parse-json
       (map-indexed (fn [idx call-log]
                      (-> call-log
                          (as-> call-log
                                (reduce (fn [altered-map [k v]]
                                          (assoc altered-map
                                                 (kebab/->kebab-case-keyword k)
                                                 v))
                                        {}
                                        call-log))
                          (assoc :log-index idx)
                          (update :function kebab/->kebab-case-keyword)
                          (update :type kebab/->kebab-case-keyword)
                          (update :context adapt-context)
                          (as-> call-log
                                (cond-> call-log
                                  (#{:fn-call} (:type call-log))
                                  (assoc :args
                                         (adapt-arg-list-for-func (:args call-log) (:context call-log) (:function call-log)))

                                  (#{:return-procedure} (:type call-log))
                                  (assoc :procedure? true)

                                  (#{:return-function} (:type call-log))
                                  (assoc :procedure? false)

                                  (#{:return-procedure :return-function} (:type call-log))
                                  (-> (assoc :ret
                                             (adapt-ret-for-func (:ret call-log) (:context call-log) (:function call-log)))
                                      (assoc :type :return)))))))
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


