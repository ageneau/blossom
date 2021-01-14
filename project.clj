(defproject ageneau/blossom "0.1.5-SNAPSHOT"
  :description "Edmonds's blossom algorithm for maximum weight matching in undirected graphs"
  :url "https://github.com/ageneau/blossom"
  :license {:name "BSD 3-Clause \"New\" or \"Revised\" License"
            :url "https://opensource.org/licenses/BSD-3-Clause"
            :year 2020
            :key "bsd-3-clause"}

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [aysylu/loom "1.0.2"]
                 [camel-snake-kebab "0.4.1"]
                 [ageneau/ageneau.utils "0.1.1"]]

  :repositories [["github" "https://maven2.github.com"]]

  :deploy-repositories [["releases" :clojars]]


  :profiles {:dev
             {:dependencies [[org.clojure/clojurescript "1.10.773" :scope "provided"]
                             [com.bhauman/figwheel-main "0.2.11"]
                             [com.bhauman/rebel-readline-cljs "0.1.4"]
                             [com.taoensso/tufte "2.2.0"]
                             [criterium "0.4.6"]
                             [org.clojure/math.combinatorics "0.1.6"]]

              :global-vars {;; *warn-on-reflection* true
                            ;; *unchecked-math* :warn-on-boxed
                            *assert* true}
              :resource-paths ["test/resources"]}
             :bench
             {:main blossom.bench/-main
              :dependencies [[com.taoensso/tufte "2.2.0"]
                             [criterium "0.4.6"]]
              :source-paths ["src" "test"]
              :resource-paths ["test/resources"]
              :global-vars {*warn-on-reflection* true
                            *unchecked-math* :warn-on-boxed
                            *assert* true}}}

  :plugins [[lein-doo "0.1.11"]
            [lein-cloverage "1.0.13"]
            [lein-shell "0.5.0"]
            [lein-ancient "0.6.15"]
            [lein-changelog "0.3.2"]
            [lein-cljsbuild "1.1.5"]]

  :doo {:build "test"}

  :aliases {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
            "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "update-readme-version" ["shell" "sed" "-i" "s/\\\\[ageneau\\\\/blossom \"[0-9.]*\"\\\\]/[ageneau\\\\/blossom \"${:version}\"]/" "README.md"]}
  
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
                        :target :nodejs}}]}

  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["changelog" "release"]
                  ["update-readme-version"]
                  ["vcs" "commit"]
                  ["vcs" "tag"]
                  ["deploy"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]])

