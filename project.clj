(defproject ageneau/blossom "0.1.0"
  :description "Edmonds's blossom algorithm for maximum weight matching in undirected graphs"
  :url "https://github.com/ageneau/blossom"
  :license {:name "BSD 3-Clause \"New\" or \"Revised\" License"
            :url "https://opensource.org/licenses/BSD-3-Clause"
            :year 2020
            :key "bsd-3-clause"}

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.520" :scope "provided"]
                 [aysylu/loom "1.0.2"]
                 [camel-snake-kebab "0.4.0"]]

  :repositories [["github" "https://maven2.github.com"]
                 ["releases" {:url "https://repo.clojars.org"
                              :creds :gpg}]]

  :profiles {:dev
             {:dependencies [[lein-doo "0.1.11"]
                             [com.bhauman/figwheel-main "0.2.0"]
                             [com.bhauman/rebel-readline-cljs "0.1.4"]
                             [com.taoensso/tufte "2.0.1"]
                             [criterium "0.4.4"]
                             [org.clojure/math.combinatorics "0.1.4"]]
              :plugins      [[lein-doo "0.1.11"]]
              :global-vars {;; *warn-on-reflection* true
                            ;; *unchecked-math* :warn-on-boxed
                            *assert* true}
              :resource-paths ["test/resources"]}
             :bench
             {:main blossom.bench/-main
              :dependencies [[com.taoensso/tufte "2.0.1"]
                             [criterium "0.4.4"]]
              :source-paths ["src" "test"]
              :resource-paths ["test/resources"]
              :global-vars {*warn-on-reflection* true
                            *unchecked-math* :warn-on-boxed
                            *assert* true}}}

  :plugins [[lein-cljsbuild "1.1.5"]]

  :doo {:build "test"}

  :aliases {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
            "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]}
  
  :cljsbuild
  {:builds [{:id "test"
             :source-paths ["src" "test" "target/classes"]
             :compiler {:output-to "target/js/testable.js"
                        :output-dir "target/js/out"
                        :main blossom.test-runner
                        :optimizations :none
                        :pretty-print  true
                        :source-map true}}
            {:id "node-test"
             :source-paths ["src" "test" "target/classes"]
             :compiler {:output-to "target/nodejs/testable.js"
                        :output-dir "target/nodejs/out"
                        :main blossom.test-runner
                        :optimizations :none
                        :pretty-print  true
                        :source-map true
                        :target :nodejs}}]})

