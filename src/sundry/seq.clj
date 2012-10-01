(ns sundry.seq)

(defn interleave-all [c1 c2]
  (lazy-seq
   (let [s1 (seq c1)
         s2 (seq c2)]
     (cond
      (and s1 s2) (cons (first s1)
                        (cons (first s2)
                              (interleave-all (rest s1) (rest s2))))
      s1 s1
      s2 s2))))

(defn distinct-key
  "Returns a lazy sequence of the elements of coll with (k element)
  duplicates removed"
  [k coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[f :as xs] seen]
                   (when-let [s (seq xs)]
                     (let [key (k f)]
                       (if (contains? seen key) 
                         (recur (rest s) seen)
                         (cons f (step (rest s) (conj seen key)))))))
                 xs seen)))]
    (step coll #{})))

(defn index-by
  [keyfn rows & {:keys [unique many seed] :or {unique false, many false, seed {}}}]
  (reduce
   (fn [m [row k]]
     (if unique
       (assoc m k row)
       (assoc m k (conj (m k []) row))))
   seed
   (if many
     (for [row rows
           sub-k (keyfn row)]
       [row sub-k])
     (map (juxt identity keyfn) rows))))
