(ns util.core)

(defn mapply [f & args]
  (apply f (apply concat (butlast args) (last args))))

(defn full-name [x]
  (str (when-let [ns (namespace x)]
         (str (namespace x) "/"))
       (name x)))