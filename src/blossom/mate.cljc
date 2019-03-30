(ns blossom.mate)

(defprotocol IMate
  (mate-assoc [this v1 v2])
  (mate [this v]))

(extend-type blossom.context.Context
  IMate
  (mate-assoc [this v1 v2]
    (update this :mate assoc v1 v2))

  (mate [this v]
    (nth (:mate this) v)))
