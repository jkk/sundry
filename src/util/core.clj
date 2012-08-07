(ns util.core)

(defn mapply [f & args]
  (apply f (apply concat (butlast args) (last args))))