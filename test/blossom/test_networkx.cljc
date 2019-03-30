(ns blossom.test-networkx
  (:require [blossom.max-weight-matching :as mwm]
            [blossom.networkx :as networkx]
            [blossom.primal-dual :as pdual]
            [blossom.test-utils :as utils]
            [blossom.specs]
            [clojure.spec.alpha :as s]
            #?(:clj [clojure.test :refer (is deftest testing)]
               :cljs [cljs.test :refer (is deftest testing)])))

(def TEST-NAMES ["10_empty"
                 "11_singleedge"
                 "12"
                 "13"
                 "14_maxcard"
                 "15_float"
                 "16_negative"
                 "20_sblossom"
                 "21_tblossom"
                 "22_s_nest"
                 "23_s_relabel_nest"
                 "24_s_nest_expand"
                 "25_s_t_expand"
                 "26_s_nest_t_expand"
                 "30_tnasty_expand"
                 "31_tnasty2_expand"
                 "32_t_expand_leastslack"
                 "33_nest_tnasty_expand"
                 "34_nest_relabel_expand"])

(defn test-file [test-name]
  (utils/read-resource (str test-name ".json")))

(def TEST-FUNCTIONS {:augment-matching {:func #'mwm/augment-matching}
                     :assign-label {:func #'mwm/assign-label}
                     :scan-blossom {:func #'mwm/scan-blossom}
                     :expand-blossom {:func #'mwm/expand-blossom}
                     :add-blossom {:func #'mwm/add-blossom}
                     :augment-blossom {:func #'mwm/augment-blossom}
                     :promote-sub-blossoms-to-top-blossoms {:func #'mwm/promote-sub-blossoms-to-top-blossoms}
                     :move-to-base-relabeling {:func #'mwm/move-to-base-relabeling}
                     :move-back-to-entry-child-relabeling {:func #'mwm/move-back-to-entry-child-relabeling}
                     :scan-neighbors {:func #'mwm/scan-neighbors
                                      :impure-func? true}
                     :find-augmenting-path {:func #'mwm/find-augmenting-path
                                            :impure-func? true}
                     :compute-delta {:func #'pdual/compute-delta
                                     :impure-func? true}
                     :verify-optimum {:func #'mwm/verify-optimum}
                     :max-weight-matching {:func #'mwm/max-weight-matching-impl
                                           :python-func-name "max-weight-matching"
                                           :impure-func? true}
                     })

(def ALL-RESULTS (->> TEST-NAMES
                      (map test-file)
                      (filter some?)
                      (map (fn [file]
                             (->> file
                                  networkx/read-log
                                  (map (fn [[id call-log]]
                                         [id (assoc call-log :file file)]))
                                  (into {}))))
                      (reduce merge)
                      (reduce (fn [out [id call-logs]]
                                (let [fname (get-in call-logs [:fn-call :function])]
                                  (assoc-in out [fname :tests id] call-logs)))
                              {})))

(defn find-result [func-name id]
  (get-in ALL-RESULTS [func-name :tests id]))

(defn get-func-name [test-result]
  (get-in test-result [:fn-call :function]))

(defn clojure-function [fname-kw]
  (get-in TEST-FUNCTIONS [fname-kw :func]))

(defmulti test-function-args (fn [atest extra-args] (get-func-name atest)))

(defmethod test-function-args :default
  [atest extra-args]
  (let [args (get-in atest [:fn-call :args])
        context-pre (get-in atest [:fn-call :context])]
    (vec (concat [context-pre] args extra-args))))

(defmethod test-function-args :max-weight-matching
  [atest extra-args]
  (let [[edges opts] (get-in atest [:fn-call :args])]
    #_(clojure.pprint/pprint {:edges edges
                              :opts opts
                              :extra extra-args})
    [edges (merge opts extra-args)]))

(defn run-test
  ([func-name id {:keys [dry-run python-func-name supports-logging?]
                  :or {python-func-name (name func-name)}}]
   (let [atest (find-result func-name id)
         func (clojure-function func-name)
         ret-log (atom [])
         extra-args (cond-> {}
                      supports-logging?
                      (assoc :ret-log ret-log))
         ret-clojure (when-not dry-run
                       (apply func (test-function-args atest extra-args)))]
     (-> atest
         (assoc :ret-clojure ret-clojure)
         (assoc :ret-log @ret-log))))
  ([func-name id]
   (run-test func-name id {})))

(defn function-type [test-result]
  (let [procedure? (get-in test-result [:return :procedure?])
        func-name (get-func-name test-result)]
    (cond
      (get-in TEST-FUNCTIONS [func-name :impure-func?]) :impure-function
      procedure? :procedure
      :else :pure-function)))

(defn test-id [test-result]
  (get-in test-result [:fn-call :id]))

(defn python-return-value [test-result]
  (get-in test-result [:return :ret]))

(defn python-input-context [test-result]
  (get-in test-result [:fn-call :context]))

(defn python-return-context [test-result]
  (get-in test-result [:return :context]))

(defmulti clojure-return-value get-func-name)

(defmethod clojure-return-value :default
  [test-result]
  (get test-result :ret-clojure))

(defmethod clojure-return-value :scan-neighbors
  [test-result]
  (:augmented (get test-result :ret-clojure)))

(defmethod clojure-return-value :find-augmenting-path
  [test-result]
  (:augmented (get test-result :ret-clojure)))

(defmethod clojure-return-value :compute-delta
  [test-result]
  (dissoc (get test-result :ret-clojure) :context))

(defmethod clojure-return-value :max-weight-matching
  [test-result]
  (get-in test-result [:ret-clojure :result]))

(defmulti clojure-return-context get-func-name)

(defmethod clojure-return-context :default
  [test-result]
  (get test-result :ret-clojure))

(defmethod clojure-return-context :scan-neighbors
  [test-result]
  (get-in test-result [:ret-clojure :context]))

(defmethod clojure-return-context :find-augmenting-path
  [test-result]
  (get-in test-result [:ret-clojure :context]))

(defmethod clojure-return-context :compute-delta
  [test-result]
  (get-in test-result [:ret-clojure :context]))

(defmethod clojure-return-context :max-weight-matching
  [test-result]
  (-> test-result
      (get-in [:ret-clojure :context])
      (update :options dissoc :ret-log)))

(defn perform-checks [test-result]
  (let [[test-id
         python-input-context
         python-return-context
         python-return-value
         clojure-return-context
         clojure-return-value]
        ((juxt test-id
               python-input-context
               python-return-context
               python-return-value
               clojure-return-context
               clojure-return-value)
         test-result)
        ftype (function-type test-result)]
    (is (nil? (s/explain-data :context/context python-input-context))
        (str "Invalid python-input-context for id " test-id))
    (is (nil? (s/explain-data :context/context python-return-context))
        (str "Invalid python-return-context for id " test-id))
    (when-not (= :pure-function ftype)
      (is (nil? (s/explain-data :context/context clojure-return-context))
          (str "Invalid clojure-return-context for id " test-id))
      (is (= python-return-context clojure-return-context)
          (str "Unexpected clojure-return-context for id " test-id)))
    (when (= :pure-function ftype)
      (is (= python-input-context python-return-context)
          (str "Unexpected python-return-context context for id " test-id)))

    (when-not (= :procedure ftype)
      (is (= python-return-value clojure-return-value)
          (str "Unexpected clojure-return-value for id " test-id)))))

(defn test-networkx-function
  [[fname-kw test-info]]
  (testing (str "Function: " (name fname-kw))
    (doseq [test (vals (get-in ALL-RESULTS [fname-kw :tests]))]
      (let [id (get-in test [:fn-call :id])]
        (testing (str "Test ID: " id)
          (let [test-result (run-test fname-kw id test-info)]
            (perform-checks test-result)))))))

(deftest networkx-tests
  (testing "Check that our implementation works the same as networkx"
    (doseq [function TEST-FUNCTIONS]
      (test-networkx-function function))))
