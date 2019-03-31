(ns blossom.mate)

(defprotocol PMate
  (set-mate [this v1 v2])
  (mate [this v]))

(extend-type blossom.context.Context
  PMate
  (set-mate [this v1 v2]
    (update this :mate assoc v1 v2))

  (mate [this v]
    (nth (:mate this) v)))
