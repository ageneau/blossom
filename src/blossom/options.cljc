(ns blossom.options)

(defprotocol IOptions
  (get-option [this opt]))

(extend-type blossom.context.Context
  IOptions
  (get-option [this opt]
    (get (:options this) opt)))
