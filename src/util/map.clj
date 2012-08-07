(ns util.map)

(defn merge-missing
  "Add entries to m1 from m2 that aren't already present"
  [m1 m2]
  (reduce
   (fn [m1 [k v]]
     (if (contains? m1 k)
       m1
       (assoc m1 k v)))
   m1 m2))

(defn assoc-unless [pred m & kvs]
  (reduce
   (fn [m [k v]]
     (if (pred v)
       m
       (assoc m k v)))
   m (partition 2 kvs)))
