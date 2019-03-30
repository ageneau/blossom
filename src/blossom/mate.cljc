(ns blossom.mate)

(defprotocol PMate
  (mate-assoc [this v1 v2])
  (mate [this v]))

(extend-type blossom.context.Context
  PMate
  (mate-assoc [this v1 v2]
    (update this :mate assoc v1 v2))

  (mate [this v]
    (nth (:mate this) v)))
