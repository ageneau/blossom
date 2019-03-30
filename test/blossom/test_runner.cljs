(ns blossom.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [blossom.blossom-test]
   [blossom.test-networkx]))

(enable-console-print!)

(doo-tests 'blossom.test-networkx
           'blossom.blossom-test)

