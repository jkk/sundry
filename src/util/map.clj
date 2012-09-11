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

(defn expand-keys [m]
  (reduce-kv
    (fn [m k v]
      (if (sequential? k)
        (merge (dissoc m k)
               (zipmap k (if (sequential? v)
                           v (repeat v))))
        m))
    m m))

#_(defn assoc-unless [pred m & kvs]
  (reduce
   (fn [m [k v]]
     (if (pred v)
       m
       (assoc m k v)))
   m (partition 2 kvs)))

(defn assoc-if [m pred & kvs]
  (reduce
   (fn [m [k v]]
     (if (pred m k v)
       (assoc m k v)
       m))
   m
   (partition 2 kvs)))

(defn assoc-present [m & kvs]
  (apply assoc-if m
         (fn [m k _]
           (contains? m k))
         kvs))

(defn assoc-absent [m & kvs]
  (apply assoc-if m
         (fn [m k _]
           (not (contains? m k)))
         kvs))

(defn update-if [m pred k f & args]
  (if (pred m k)
    (apply update-in m [k] f args)
    m))

(defn update-present [m k f & args]
  (apply update-if m (fn [m k]
                       (contains? m k))
         k f args))

(defn update-absent [m k f & args]
  (apply update-if m (fn [m k]
                       (not (contains? m k)))
         k f args))