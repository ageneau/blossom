(ns blossom.context)

(defrecord Context [edges
                    nedge
                    nvertex
                    endpoint
                    neighbend
                    integer-weights?
                    mate
                    label
                    label-end
                    in-blossom
                    blossom-parent
                    blossom-childs
                    blossom-base
                    blossom-endps
                    best-edge
                    blossom-best-edges
                    unused-blossoms
                    dual-var
                    allow-edge
                    queue
                    options])
