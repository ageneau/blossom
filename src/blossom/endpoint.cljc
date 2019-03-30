(ns blossom.endpoint
  (:require [blossom.constants :as c]))

(def no-endp? #(= c/NO-ENDP %))
(def some-endp? #(not= c/NO-ENDP %))

(defprotocol PEndpoint
  (vertex [g p] "Vertex to which endpoint p is attached.")
  (vertex-endpoints [g v] "Collection of remote endpoints of the edges attached to v.")
  (edge [g p] "Edge to which endpoint p is attached")
  (opposite [g p] "Opposite endpoint attached to the same edge as p")
  (opposite-vertex [g p] "Vertex to which p's partner endpoint is attached."))

(extend-type blossom.context.Context
  PEndpoint
  (vertex [g p]
    (nth (:endpoint g) p))

  (vertex-endpoints [g v]
    (nth (:neighbend g) v))

  (edge [g p]
    (quot p 2))

  (opposite [g p]
    (bit-xor p 1))

  (opposite-vertex [g p]
    (vertex g (opposite g p))))
