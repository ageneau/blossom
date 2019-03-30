(ns blossom.bench
  (:require [blossom.max-weight-matching :as mwm])
  #?(:clj (:gen-class)))

(def MIN-SIZE 2)
(def MAX-SIZE 300)

(defmacro elapsed-time
  "Evaluates expr and returns the time it took and the value of
  expr as a map with keys time and value"
  {:added "1.0"}
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     {:time (/ (double (- (. System (nanoTime)) start#)) 1000000.0)
      :value ret#}))

(defn complete-graph [n]
  (let [nedge (reduce + (range n))]
    (vec (for [i (range 0 (dec n))
               j (range (inc i) n)] [i j (rand-int nedge)]))))

(defn test-graph [n]
  (let [edges (complete-graph n)
        {:keys [time value]} (elapsed-time (doall (mwm/max-weight-matching  edges {:max-cardinality true})))]
    (println (format "%5d %f" n (/ time 1000)))))

(defn -main
  "Simple benchmark"
  [& args]
  (test-graph 100)
  #_(doseq [n (range MIN-SIZE (inc MAX-SIZE))]
    (test-graph n)))
