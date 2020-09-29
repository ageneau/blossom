(ns blossom.test-utils
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [taoensso.tufte :as tufte]))

(defn parse-json[file]
  (json/read-str file))

(defn read-resource [fn]
  (when-some [resource (io/resource fn)]
    (slurp resource)))

(defmacro elapsed-time
  "Evaluates expr and returns the time it took and the value of
  expr as a map with keys time and value"
  {:added "1.0"}
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     {:time (/ (double (- (. System (nanoTime)) start#)) 1000000.0)
      :value ret#}))

(defn profile-fn-call
  "Profiles a single call to a function f with args. 'name' is the
symbol name of the function."
  [name f args]
  (tufte/p name (apply f args)))

(defn profile-var*
  "If the specified Var holds an IFn and is not marked as a macro, its
  contents is replaced with a version wrapped in a profiling call;
  otherwise nothing happens. Can be undone with unprofile-var.
  In the unary case, v should be a Var object or a symbol to be
  resolved in the current namespace.
  In the binary case, ns should be a namespace object or a symbol
  naming a namespace and s a symbol to be resolved in that namespace."
  ([ns s]
   (profile-var* (ns-resolve ns s)))
  ([v]
   (let [^clojure.lang.Var v (if (var? v) v (resolve v))
         ns (.ns v)
         s  (.sym v)]
     (if (and (ifn? @v) (-> v meta :macro not) (-> v meta ::profiled not))
       (let [f @v
             vname (symbol (str ns "/" s))]
         (doto v
           (alter-var-root #(fn profiling-wrapper [& args]
                              (profile-fn-call vname % args)))
           (alter-meta! assoc ::profiled f)))))))


(defn unprofile-var*
  "Reverses the effect of profile-var / profile-vars / profile-ns for the
  given Var, replacing the profiled function with the original, unprofiled
  version. No-op for non-profiled Vars.
  Argument types are the same as those for profile-var."
  ([ns s]
   (unprofile-var* (ns-resolve ns s)))
  ([v]
   (let [^clojure.lang.Var v (if (var? v) v (resolve v))
         ns (.ns v)
         s  (.sym v)
         f  ((meta v) ::profiled)]
     (when f
       (doto v
         (alter-var-root (constantly ((meta v) ::profiled)))
         (alter-meta! dissoc ::profiled))))))

(defmacro profile-vars
  "Profile each of the specified Vars.
  The arguments may be Var objects or symbols to be resolved in the current
  namespace."
  [& vs]
  `(do ~@(for [x vs]
           `(if (var? ~x)
              (profile-var* ~x)
              (profile-var* (quote ~x))))))

(defmacro unprofile-vars
  "Unprofile each of the specified Vars.
  Reverses the effect of profile-var / profile-vars / profile-ns for each
  of the arguments, replacing the profiled functions with the original,
  unprofiled versions."
  [& vs]
  `(do ~@(for [x vs]
           `(if (var? ~x)
              (unprofile-var* ~x)
              (unprofile-var* (quote ~x))))))


(defmacro doprofile
  "Given a sequence of function identifiers, evaluate the body
  expressions in an environment in which the identifiers are bound to
  the profiled functions. Does not work on inlined functions,
  such as clojure.core/+"
  [fnames opts & exprs]
  `(try
     (profile-vars ~@fnames)
     (tufte/profiled
      ~opts
      ~@exprs)
     (finally (unprofile-vars ~@fnames))))

