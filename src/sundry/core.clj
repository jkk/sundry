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