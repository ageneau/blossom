(ns blossom.blossom-test
  (:require [blossom.constants :as c]
            [blossom.graph :as graph]
            [blossom.matching
             :refer
             [is-matching?
              is-maximal-matching?
              is-perfect-matching?
              maximal-matching]]
            [blossom.max-weight-matching :refer [max-weight-matching]]
            [blossom.specs]
            [blossom.test-utils :as utils]
            [clojure.math.combinatorics :as combo]
            #?(:clj [clojure.test :refer (is deftest testing)]
               :cljs [cljs.test :refer (is deftest testing)])
            [clojure.test :refer [deftest is testing]]
            [loom.graph :as lg]))

(deftest test-10-empty
  (testing "empty input graph"
    (let [edges []]
      (is (= #{} (max-weight-matching edges))))))

(deftest test-11-singleedge
  (testing "single edge"
    (let [edges [[0 1 1]]]
      (is (= #{#{0 1}} (max-weight-matching edges))))))

(deftest test-12
  (testing "test-12"
    (let [edges [[1 2 10] [2 3 11]]]
      (is (= #{#{2 3}} (max-weight-matching edges))))))

(deftest test-13
  (testing "test-13"
    (let [edges [[1 2 5] [2 3 11] [3 4 5]]]
      (is (= #{#{2 3}} (max-weight-matching edges))))))

(deftest test-14-maxcard
  (testing "maximum cardinality"
    (let [edges [[1 2 5] [2 3 11] [3 4 5]]]
      (is (= #{#{4 3} #{1 2}}
             (max-weight-matching edges {:max-cardinality true}))))))

(deftest test-15-float
  (testing "floating point weigths"
    (let [edges [[1 2 Math/PI]
                 [2 3 (Math/exp 1)]
                 [1 3 3.0]
                 [1 4 (Math/sqrt 2.0)]]]
      (is (= #{#{1 4} #{3 2}}
             (max-weight-matching edges))))))

(deftest test-16-negative
  (testing "negative weights"
    (let [edges [[1 2 2]
                 [1 3 -2]
                 [2 3 1]
                 [2 4 -1]
                 [3 4 -6]]]
      (is (= #{#{1 2}}
             (max-weight-matching edges {:max-cardinality false})))
      (is (= #{#{4 2} #{1 3}}
             (max-weight-matching edges {:max-cardinality true}))))))

(deftest test-20-sblossom
  (testing "create S-blossom and use it for augmentation"
    (let [edges1 [[1 2 8]
                  [1 3 9]
                  [2 3 10]
                  [3 4 7]]
          edges2 [[1 2 8]
                  [1 3 9]
                  [2 3 10]
                  [3 4 7]
                  [1 6 5]
                  [4 5 6]]]
      (is (= #{#{4 3} #{1 2}}
             (max-weight-matching edges1)))
      (is (= #{#{1 6} #{4 5} #{3 2}}
             (max-weight-matching edges2))))))

(deftest test-21-tblossom
  (testing "create S-blossom, relabel as T-blossom, use for augmentation"
    (let [edges1 [[1 2 9]
                  [1 3 8]
                  [2 3 10]
                  [1 4 5]
                  [4 5 4]
                  [1 6 3]]
          edges2 [[1 2 9]
                  [1 3 8]
                  [2 3 10]
                  [1 4 5]
                  [4 5 3]
                  [1 6 4]]
          edges3 [[1 2 9]
                  [1 3 8]
                  [2 3 10]
                  [1 4 5]
                  [4 5 3]
                  [3 6 4]]]
      (is (= #{#{1 6} #{4 5} #{3 2}}
             (max-weight-matching edges1)))
      (is (= #{#{1 6} #{4 5} #{3 2}}
             (max-weight-matching edges2)))
      (is (= #{#{6 3} #{1 2} #{4 5}}
             (max-weight-matching edges3))))))

(deftest test-22-s-nest
  (testing "create nested S-blossom, use for augmentation"
    (let [edges [[1 2 9]
                 [1 3 9]
                 [2 3 10]
                 [2 4 8]
                 [3 5 8]
                 [4 5 10]
                 [5 6 6]]]
      (is (= #{#{6 5} #{4 2} #{1 3}}
             (max-weight-matching edges))))))

(deftest test-23-s-relabel-nest
  (testing "create S-blossom, relabel as S, include in nested S-blossom"
    (let [edges [[1 2 10]
                 [1 7 10]
                 [2 3 12]
                 [3 4 20]
                 [3 5 20]
                 [4 5 25]
                 [5 6 10]
                 [6 7 10]
                 [7 8 8]]]
      (is (= #{#{4 3} #{6 5} #{1 2} #{7 8}}
             (max-weight-matching edges))))))

(deftest test-24-s-nest-expand
  (testing "create nested S-blossom, augment, expand recursively"
    (let [edges [[1 2 8]
                 [1 3 8]
                 [2 3 10]
                 [2 4 12]
                 [3 5 12]
                 [4 5 14]
                 [4 6 12]
                 [5 7 12]
                 [6 7 14]
                 [7 8 12]]]
      (is (= #{#{3 5} #{4 6} #{1 2} #{7 8}}
             (max-weight-matching edges))))))

(deftest test-25-s-t-expand
  (testing "create S-blossom, relabel as T, expand"
    (let [edges [[1 2 23]
                 [1 5 22]
                 [1 6 15]
                 [2 3 25]
                 [3 4 22]
                 [4 5 25]
                 [4 8 14]
                 [5 7 13]]]
      (is (= #{#{7 5} #{4 8} #{1 6} #{3 2}}
             (max-weight-matching edges))))))

(deftest test-26-s-nest-t-expand
  (testing "create nested S-blossom, relabel as T, expand"
    (let [edges [[1 2 19]
                 [1 3 20]
                 [1 8 8]
                 [2 3 25]
                 [2 4 18]
                 [3 5 18]
                 [4 5 13]
                 [4 7 7]
                 [5 6 7]]]
      (is (= #{#{6 5} #{1 8} #{7 4} #{3 2}}
             (max-weight-matching edges))))))

(deftest test-30-tnasty-expand
  (testing "create blossom, relabel as T in more than one way, expand, augment"
    (let [edges [[1 2 45]
                 [1 5 45]
                 [2 3 50]
                 [3 4 45]
                 [4 5 50]
                 [1 6 30]
                 [3 9 35]
                 [4 8 35]
                 [5 7 26]
                 [9 10 5]]]
      (is (= #{#{7 5} #{4 8} #{1 6} #{9 10} #{3 2}}
             (max-weight-matching edges))))))

(deftest test-31-s-nest-t-expand
  (testing "again but slightly different"
    (let [edges [[1 2 45]
                 [1 5 45]
                 [2 3 50]
                 [3 4 45]
                 [4 5 50]
                 [1 6 30]
                 [3 9 35]
                 [4 8 26]
                 [5 7 40]
                 [9 10 5]]]
      (is (= #{#{7 5} #{4 8} #{1 6} #{9 10} #{3 2}}
             (max-weight-matching edges))))))

(deftest test-32-s-nest-t-expand
  (testing "create blossom, relabel as T, expand such that a new least-slack S-to-free edge is produced, augment"
    (let [edges [[1 2 45]
                 [1 5 45]
                 [2 3 50]
                 [3 4 45]
                 [4 5 50]
                 [1 6 30]
                 [3 9 35]
                 [4 8 28]
                 [5 7 26]
                 [9 10 5]]]
      (is (= #{#{7 5} #{4 8} #{1 6} #{9 10} #{3 2}}
             (max-weight-matching edges))))))

(deftest test-33-s-nest-t-expand
  (testing "create nested blossom, relabel as T in more than one way, expand outer blossom such that inner blossom ends up on an augmenting path"
    (let [edges [[1 2 45]
                 [1 7 45]
                 [2 3 50]
                 [3 4 45]
                 [4 5 95]
                 [4 6 94]
                 [5 6 94]
                 [6 7 50]
                 [1 8 30]
                 [3 11 35]
                 [5 9 36]
                 [7 10 26]
                 [11 12 5]]]
      (is (= #{#{7 10} #{9 5} #{1 8} #{4 6} #{12 11} #{3 2}}
             (max-weight-matching edges))))))

(deftest test-34-s-nest-t-expand
  (testing "create nested S-blossom, relabel as S, expand recursively"
    (let [edges [[1 2 40]
                 [1 3 40]
                 [2 3 60]
                 [2 4 55]
                 [3 5 55]
                 [4 5 50]
                 [1 8 15]
                 [5 7 30]
                 [7 6 10]
                 [8 10 10]
                 [4 9 30]]]
      (is (= #{#{7 6} #{3 5} #{10 8} #{1 2} #{4 9}}
             (max-weight-matching edges))))))

(defn complete-graph [n]
  (vec (for [i (range 0 (dec n))
             j (range (inc i) n)] [i j 1])))

(deftest test-complete-graph
  (testing "large graph"
    (is (= 32 (count (max-weight-matching (complete-graph 65) {:max-cardinality true}))))))

(deftest test-trivial-1
  (testing "Empty graph"
    (let [g []]
      (is (= (max-weight-matching g) #{})))))

(deftest test-trivial-2
  (testing "Self loop"
    (let [g [[0 0 100]]]
      (is (= (max-weight-matching g) #{})))))

(deftest test-trivial-3
  (testing "Single edge"
    (let [g [[0 1 1]]]
      (is (= (max-weight-matching g) #{#{0 1}})))))

#_(deftest test-trivial-4
    (testing "Small graph"
      (let [g (-> (lg/weighted-graph)
                  (lg/add-edges ["one" "two" 10]
                                ["two" "three" 11]))]
        (is (= (max-weight-matching (lg/weighted-graph g)) #{#{"three" "two"}})))))

(deftest test-trivial-5
  (testing "Path"
    (let [g [[1 2 5]
             [2 3 11]
             [3 4 5]]]
      (is (= (max-weight-matching g) #{#{3 2}}))
      (is (= (max-weight-matching g {:max-cardinality true}) #{#{1 2} #{3 4}})))))

(deftest test-floating-point-weights
  (testing "Floating point weights"
    (let [g [[1 2 Math/PI]
             [2 3 (Math/exp 1)]
             [1 3 3.0]
             [1 4 (Math/sqrt 2)]]]
      (is (= (max-weight-matching g) #{#{1 4} #{2 3}})))))

(deftest negative-weights
  (testing "Negative weights"
    (let [g [[1 2 2] [1 3 -2] [2 3 1]
             [2 4 -1] [3 4 -6]]]
      (is (= (max-weight-matching g) #{#{1 2}}))
      (is (= (max-weight-matching g {:max-cardinality true}) #{#{4 2} #{1 3}})))))

(deftest s-blossom
  (testing "Create S-blossom and use it for augmentation"
    (let [g [[1 2 8] [1 3 9]
             [2 3 10] [3 4 7]]]
      (is (= (max-weight-matching g) #{#{1 2} #{3 4}}))
      (is (= (max-weight-matching (concat g [[1 6 5] [4 5 6]]))
             #{#{1 6} #{2 3} #{4 5}})))))

(deftest s-t-blossom
  (testing "Create S-blossom, relabel as T-blossom, use for augmentation"
    (let [g [[1 2 9] [1 3 8] [2 3 10]
             [1 4 5] [4 5 4] [1 6 3]]]
      (is (= (max-weight-matching g) #{#{1 6} #{2 3} #{4 5}}))
      (is (= (max-weight-matching (as-> g g
                                    (remove #{[1 6 3]} g)
                                    (conj g [3 6 4])))
             #{#{1 2} #{3 6} #{4 5}})))))

(deftest nested-s-blossom
  (testing "Create nested S-blossom, use for augmentation"
    (let [g [[1 2 9] [1 3 9] [2 3 10]
             [2 4 8] [3 5 8] [4 5 10]
             [5 6 6]]]
      (is (= (max-weight-matching g) #{#{6 5} #{4 2} #{1 3}})))))

(deftest nested-s-blossom-relabel
  (testing "Create S-blossom, relabel as S, include in nested S-blossom"
    (let [g [[1 2 10] [1 7 10] [2 3 12]
             [3 4 20] [3 5 20] [4 5 25]
             [5 6 10] [6 7 10] [7 8 8]]]
      (is (= (max-weight-matching g) #{#{1 2} #{3 4} #{5 6} #{7 8}})))))

(deftest nested-s-blossom-expand
  (testing "Create nested S-blossom, augment, expand recursively"
    (let [g [[1 2 8] [1 3 8] [2 3 10]
             [2 4 12] [3 5 12] [4 5 14]
             [4 6 12] [5 7 12] [6 7 14]
             [7 8 12]]]
      (is (= (max-weight-matching g) #{#{1 2} #{3 5} #{4 6} #{7 8}})))))

(deftest s-blossom-relabel-expand
  (testing "Create S-blossom, relabel as T, expand"
    (let [g [[1 2 23] [1 5 22] [1 6 15]
             [2 3 25] [3 4 22] [4 5 25]
             [4 8 14] [5 7 13]]]
      (is (= (max-weight-matching g) #{#{1 6} #{2 3} #{4 8} #{5 7}})))))

(deftest nested-s-blossom-relabel-expand
  (testing "Create nested S-blossom, relabel as T, expand"
    (let [g [[1 2 19] [1 3 20] [1 8 8]
             [2 3 25] [2 4 18] [3 5 18]
             [4 5 13] [4 7 7] [5 6 7]]]
      (is (= (max-weight-matching g) #{#{1 8} #{2 3} #{4 7} #{5 6}})))))

(deftest nasty-blossom-1
  (testing "Create blossom, relabel as T in more than one way, expand, augment."
    (let [g [[1 2 45] [1 5 45] [2 3 50]
             [3 4 45] [4 5 50] [1 6 30]
             [3 9 35] [4 8 35] [5 7 26]
             [9 10 5]]]
      (is (= (max-weight-matching g) #{#{7 5} #{4 8} #{1 6} #{9 10} #{3 2}})))))

(deftest nasty-blossom-2
  (testing "Again but slightly different"
    (let [g [[1 2 45] [1 5 45] [2 3 50]
             [3 4 45] [4 5 50] [1 6 30]
             [3 9 35] [4 8 26] [5 7 40]
             [9 10 5]]]
      (is (= (max-weight-matching g) #{#{1 6} #{2 3} #{4 8} #{5 7} #{9 10}})))))

(deftest nasty-blossom-least-slack
  (testing "Create blossom, relabel as T, expand such that a new
least-slack S-to-free dge is produced, augment"
    (let [g [[1 2 45] [1 5 45] [2 3 50]
             [3 4 45] [4 5 50] [1 6 30]
             [3 9 35] [4 8 28] [5 7 26]
             [9 10 5]]]
      (is (= (max-weight-matching g) #{#{1 6} #{2 3} #{4 8} #{5 7} #{9 10}})))))

(deftest nasty-blossom-augmenting
  (testing "Create nested blossom, relabel as T in more than one way"
    (let [g [[1 2 45] [1 7 45] [2 3 50]
             [3 4 45] [4 5 95] [4 6 94]
             [5 6 94] [6 7 50] [1 8 30]
             [3 11 35] [5 9 36] [7 10 26]
             [11 12 5]]]
      (is (= (max-weight-matching g) #{#{1 8} #{2 3} #{4 6} #{5 9} #{7 10} #{11 12}})))))

(deftest nasty-blossom-expand-recursively
  (testing "Create nested S-blossom, relabel as S, expand recursively"
    (let [g [[1 2 40] [1 3 40] [2 3 60]
             [2 4 55] [3 5 55] [4 5 50]
             [1 8 15] [5 7 30] [7 6 10]
             [8 10 10] [4 9 30]]]
      (is (= (max-weight-matching g) #{#{1 2} #{3 5} #{4 9} #{6 7} #{8 10}})))))

(deftest is-matching-path
  (is (is-matching? (as-> (lg/weighted-graph) graph
                      (apply lg/add-path graph (range 4)))
                    #{#{0 1} #{2 3}})))

(deftest is-matching-empty-matching
  (is (is-matching? (as-> (lg/weighted-graph) graph
                      (apply lg/add-path graph (range 4)))
                    #{})))

(deftest is-matching-single-edge
  (is (is-matching? (as-> (lg/weighted-graph) graph
                      (apply lg/add-path graph (range 4)))
                    #{#{1 2}})))

(deftest is-matching-valid
  (is (is-matching? (as-> (lg/weighted-graph) graph
                      (apply lg/add-path graph (range 4)))
                    #{#{0 1} #{2 3}})))

(deftest is-matching-invalid
  (is (not (is-matching? (as-> (lg/weighted-graph) graph
                           (apply lg/add-path graph (range 4)))
                         #{#{0 1} #{1 2} #{2 3}}))))

(deftest is-maximal-matching-path
  (is (is-maximal-matching? (as-> (lg/weighted-graph) graph
                              (apply lg/add-path graph (range 4)))
                            #{#{0 1} #{2 3}})))

(deftest is-maximal-matching-not-matching
  (is (not (is-maximal-matching? (as-> (lg/weighted-graph) graph
                                   (apply lg/add-path graph (range 4)))
                                 #{#{0 1} #{1 2} #{2 3}}))))

(deftest is-maximal-matching-not-maximal
  (is (not (is-maximal-matching? (as-> (lg/weighted-graph) graph
                                   (apply lg/add-path graph (range 4)))
                                 #{#{0 1}}))))

(deftest is-perfect-matching-valid
  (is (is-perfect-matching? (as-> (lg/weighted-graph) graph
                              (apply lg/add-path graph (range 4)))
                            #{#{0 1} #{2 3}})))

(deftest is-perfect-matching-not-path
  (is (is-perfect-matching? (as-> (lg/weighted-graph) graph
                              (apply lg/add-cycle graph (range 4))
                              (lg/add-edges graph
                                            [0 4]
                                            [1 4]
                                            [5 2]))
                            #{#{1 4} #{0 3} #{5 2}})))

(deftest is-perfect-matching-not-matching
  (is (not (is-perfect-matching? (as-> (lg/weighted-graph) graph
                                   (apply lg/add-path graph (range 4)))
                                 #{#{0 1} #{1 2} #{2 3}}))))

(deftest is-perfect-matching-maximal-but-not-perfect
  (is (not (is-perfect-matching? (as-> (lg/weighted-graph) graph
                                   (apply lg/add-cycle graph (range 4))
                                   (lg/add-edges graph
                                                 [0 4]
                                                 [1 4]))
                                 #{#{1 4} #{0 3}}))))

(deftest maximal-matching-valid-matching
  (let [g (-> (lg/weighted-graph)
              (lg/add-edges [1 2] [1 5] [2 3] [2 5] [3 4] [3 6] [5 6]))]
    (is (is-maximal-matching? g (maximal-matching g)))))

(deftest maximal-matching-single-edge
  ;; In the star graph, any maximal matching has just one edge.
  (let [g (graph/star-graph 5)
        matching (maximal-matching g)]
    (is (= 1 (count matching)))
    (is (is-maximal-matching? g (maximal-matching g)))))

(deftest maximal-matching-self-loops
  (testing "Create the path graph with two self-loops."
    (let [g (as-> (lg/weighted-graph) g
              (apply lg/add-path g (range 3))
              (lg/add-edges g [0 0] [1 1]))
          matching (maximal-matching g)]
      (is (= 1 (count matching)))
      (is (is-maximal-matching? g (maximal-matching g))))))

(deftest maximal-matching-ordering
  (testing "a maximal matching is computed correctly
regardless of the order in which nodes are added to the graph."
    (doseq [nodes (combo/permutations (range 3))]
      (let [g (-> (lg/weighted-graph)
                  (lg/add-nodes* nodes)
                  (lg/add-edges [0 1] [0 2]))
            matching (maximal-matching g)]
        (is (= 1 (count matching)))
        (is (is-maximal-matching? g (maximal-matching g)))))))

#_(deftest wikipedia-examples
    (testing "Examples from https://en.wikipedia.org/wiki/Matching_(lg_theory)"
      (let [g1 (as-> (lg/weighted-graph) g
                 (lg/add-edges g
                               [1 2] [1 4]
                               [2 3] [2 4] [2 5] [2 6]))
            g2 (as-> (lg/weighted-graph) g
                 (lg/add-edges g
                               [1 2] [1 4]
                               [2 3] [2 4]
                               [3 5] [3 6]
                               [4 5]))
            g3 (as-> (lg/weighted-graph) g
                 (lg/add-edges g
                               [1 2] [1 4]
                               [2 3] [2 4]
                               [3 5]
                               [4 5]))
            m1 #{#{2 4}}
            m2 #{#{1 4} #{2 3}}
            m3 #{#{1 4} #{2 3}}
            m4 #{#{1 4} #{2 3}}
            m5 #{#{1 2} #{3 6} #{4 5}}
            m6 #{#{1 4} #{2 3}}]
        (is (is-maximal-matching? g1 m1))
        (is (is-maximal-matching? g2 m2))
        (is (is-maximal-matching? g3 m3))
        (is (not (is-perfect-matching? g1 m4)))
        (is (is-perfect-matching? g2 m5))
        (is (not (is-perfect-matching? g3 m6))))))


(deftest complete-graph-1
  (testing "large graph 1"
    (let [edges (->> "large_graph1.json"
                     utils/read-resource
                     utils/parse-json)]
      (is (= #{#{46 32} #{6 42} #{12 16} #{36 10} #{21 51} #{24 19} #{40 5} #{50 29} #{44 9}
               #{52 37} #{39 35} #{33 45} #{1 48} #{7 43} #{0 54} #{13 49} #{15 31} #{4 23}
               #{17 30} #{22 14} #{41 25} #{28 53} #{2 26} #{38 18} #{3 47} #{27 8} #{34 11}}
             (max-weight-matching edges))))))

(deftest complete-graph-2
  (testing "large graph 2"
    (let [edges (->> "large_graph2.json"
                     utils/read-resource
                     utils/parse-json)]
      (is (= #{#{14 10} #{21 22} #{27 12} #{0 3} #{7 13} #{15 17} #{24 23} #{6 9} #{2 11}
               #{1 19} #{26 18} #{20 4} #{25 8} #{28 16}}
             (max-weight-matching edges))))))
