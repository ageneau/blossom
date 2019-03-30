(ns blossom.bench
  (:require [blossom.max-weight-matching :as mwm]
            [blossom.context :as ctx]
            [blossom.dual :as dual]
            [taoensso.tufte :as tufte :refer (defnp profiled profile)]
            [blossom.test-utils :as utils :refer [elapsed-time]])
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(tufte/add-basic-println-handler! {})

(def MIN-SIZE 2)
(def MAX-SIZE 300)

(defn complete-graph [n]
  (let [nedge (reduce + (range n))]
    (vec (for [i (range 0 (dec n))
               j (range (inc i) n)] [i j (rand-int nedge)]))))

(defn test-graph [n]
  (let [edges (complete-graph n)
        {:keys [time value]} (elapsed-time (doall (mwm/max-weight-matching  edges {:max-cardinality true})))]
    (println (format "%5d %f" n (/ time 1000)))))

(defn dotest [graph-nodes]
  (utils/doprofile [#'test-graph
                    #'blossom.max-weight-matching/assign-label
                    #'blossom.max-weight-matching/find-augmenting-path
                    #'blossom.max-weight-matching/scan-blossom
                    #'blossom.max-weight-matching/scan-neighbors

                    #'blossom.max-weight-matching/consider-tight-edge
                    #'blossom.max-weight-matching/consider-loose-edge-to-s-blossom
                    #'blossom.max-weight-matching/consider-loose-edge-to-free-vertex

                    #'blossom.max-weight-matching/expand-blossom
                    #'blossom.max-weight-matching/augment-blossom
                    #'blossom.max-weight-matching/expand-tight-sblossoms
                    #'blossom.max-weight-matching/calc-slack
                    #'blossom.max-weight-matching/initialize-stage
                    #'blossom.max-weight-matching/initialize-context
                    #'blossom.max-weight-matching/act-on-minimum-delta

                    #'blossom.primal-dual/compute-delta
                    #'blossom.primal-dual/compute-my-best-edges
                    #'blossom.primal-dual/get-least-slack-edges
                    #'blossom.primal-dual/update-dual-var-with-delta
                    #'blossom.primal-dual/update-blossom-dual-with-delta
                    #'blossom.primal-dual/update-best-edges
                    #'blossom.primal-dual/compute-delta-1
                    #'blossom.primal-dual/compute-delta-2
                    #'blossom.primal-dual/compute-delta-3
                    #'blossom.primal-dual/compute-delta-4

                    #'blossom.dual/slack
                    #'blossom.dual/dual-var
                    #'blossom.dual/allowed-edge?
                    #'blossom.dual/best-edge-clear

                    #'blossom.blossom/in-blossom
                    #'blossom.blossom/blossom-parent
                    #'blossom.blossom/blossom-childs
                    #'blossom.blossom/blossom-base
                    #'blossom.blossom/blossom-endps
                    #'blossom.blossom/blossom-leaves
                    #'blossom.blossom/trivial-blossom?

                    #'blossom.mate/mate
                    #'blossom.mate/mate-assoc

                    #'blossom.label/labeled-s-blossom?
                    #'blossom.label/labeled-t-blossom?
                    #'blossom.label/unlabeled?
                    #'blossom.label/labeled?
                    #'blossom.label/label-endp-assoc
                    #'blossom.label/add-label
                    
                    #'blossom.graph/initialize
                    #'blossom.graph/max-vertex-id
                    #'blossom.graph/compute-endpoint
                    #'blossom.graph/compute-neighbend
                    #'blossom.graph/max-weight
                    #'blossom.graph/integer-weights?
                    #'blossom.endpoint/vertex-endpoints
                    #'blossom.graph/edge
                    #'blossom.endpoint/vertex]
                   {}
                   (test-graph graph-nodes)))

(defn -main
  "Simple benchmark"
  [& args]
  (cond (and (seq args)
             (= "short" (first args)))
        (test-graph (or (Integer/parseInt (second args)) 100))

        (and (seq args)
             (= "profile" (first args)))
        (println (str "\n"
                      (tufte/format-pstats
                       (second
                        (dotest (or (Integer/parseInt (second args)) 100))))))

        :else (doseq [n (range MIN-SIZE (inc MAX-SIZE))]
                (test-graph n))))
