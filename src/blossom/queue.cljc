(ns blossom.queue)

(defprotocol IQueue
  (queue-push [this coll])
  (queue-peek [this])
  (queue-pop [this])
  (queue-empty? [this])
  (queue-clear [this]))

(extend-type blossom.context.Context
  IQueue
  (queue-push [this coll]
    (update this :queue into coll))

  (queue-peek [this]
    (peek (:queue this)))

  (queue-pop [this]
    (update this :queue pop))

  (queue-empty? [this]
    (empty? (:queue this)))

  (queue-clear [this]
    (update this :queue empty)))
