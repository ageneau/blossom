(ns blossom.utils)

(defn wget
  "Get index i from v, wrapping negative indices if necessary."
  [v i]
  (assert (counted? v)
          "inefficient count operation")
  (nth v (mod i (count v))))


(defn filter-and-find-min-for-key
  "Remove nil elements from sequence and find an element for
  which (get element key) is minimum"
  [key s]
  (let [filtered (filter some? s)]
    (when (seq filtered)
      (reduce (partial min-key key) (reverse filtered)))))

