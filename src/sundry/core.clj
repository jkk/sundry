(ns sundry.core)

(defn mapply [f & args]
  (apply f (apply concat (butlast args) (last args))))

(defn shift-when [pred [x & xs :as coll]]
  (if (pred x)
    [x xs]
    [nil coll]))

(defn full-name [x]
  (str (when-let [ns (namespace x)]
         (str (namespace x) "/"))
       (name x)))

;; from clojure.core.reducers
(defmacro compile-if
  "Evaluate `exp` and if it returns logical true and doesn't error, expand to
  `then`.  Else expand to `else`."
  ([exp then]
    `(compile-if ~exp ~then nil))
  ([exp then else]
    (if (try (eval exp)
          (catch Throwable _ false))
      `(do ~then)
      `(do ~else))))