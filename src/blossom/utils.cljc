(ns blossom.utils)

(defn positions
  "Returns a lazy sequence containing the positions at which pred
  is true for items in coll."
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(defn split-and-reverse
  "Split a sequence at index n and reverse the front and back
  If n is negative split at index (count coll) + n and reverse front and back."
  [coll n]
  (let [n (if (neg? n) (+ (count coll) n) n)
        [front back] (split-at n coll)]
    (concat back front)))

(defn wget
  "Get index i from v, wrapping negative indices if necessary."
  [v i]
  (assert (counted? v)
          "inefficient count operation")
  (nth v (mod i (count v))))

(defn log-add
  [ret-log id content]
  (if @ret-log
    (swap! ret-log (fnil conj []) {:id id :content content})))

(defn filter-and-find-min-for-key
  "Remove nil elements from sequence and find an element for
  which (get element key) is minimum"
  [key s]
  (let [filtered (filter some? s)]
    (when (seq filtered)
      (reduce (partial min-key key) (reverse filtered)))))

(defn doto-assert
  "Apply f to x and call `clojure.core/assert` on the result. Return x.
  Useful as an assertion method within -> threads"
  ([x f]
   (do (assert (f x)) x))
  ([x f message]
   (do x (assert (f x) message) x)))
