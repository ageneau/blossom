(ns blossom.test-utils
  (:require [blossom.utils :as sut]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defn parse-json[file]
  (json/read-str file))

(defn read-resource [fn]
  (slurp (io/resource fn)))
