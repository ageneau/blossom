(ns blossom.core
  (:require [blossom.max-weight-matching :as mwm]
            [blossom.matching :as m]))

(def API-FUNCTIONS #{`mwm/max-weight-matching
                     `m/is-matching?
                     `m/is-maximal-matching?
                     `m/is-perfect-matching?
                     `m/maximal-matching})
