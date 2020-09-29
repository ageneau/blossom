(ns blossom.test-utils
  (:require [clojure.string :as string]))

;; File utils
(defn read-file [file]
  (let [fs (js/require "fs")]
    (when (.existsSync fs file)
      (.readFileSync fs file "utf8"))))

(defn real-path [file]
  (let [fs (js/require "fs")]
    (when (.existsSync fs file)
      (.realpathSync fs file))))

(defn cwd []
  (let [process (js/require "process")]
    (.cwd process)))

(defn test-resource-dir []
  (str (cwd) "/test/resources"))

(defn remove-extension
  "Removes the file extension from a file name"
  [fn]
  (string/replace fn #"\.[^.]*$" ""))

(defn parse-json [s]
  (js->clj (js/JSON.parse s)))

(defn read-resource [fn]
  (read-file (str (test-resource-dir) "/" fn)))
