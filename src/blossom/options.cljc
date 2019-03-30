(ns blossom.options)

(defprotocol POptions
  (get-option [this opt]))

(extend-type blossom.context.Context
  POptions
  (get-option [this opt]
    (get (:options this) opt)))
