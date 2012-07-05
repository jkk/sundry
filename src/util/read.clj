(ns util.read
  (:refer-clojure :exclude [read])
  (:require
   [clojure.java.io :as io]))

(defn read
  [f]
  (binding [*read-eval* false]
    (clojure.core/read (java.io.PushbackReader. (io/reader f)))))

(defn read-seq
  [f]
  (let [eof (Object.)
        pbr (java.io.PushbackReader. (io/reader f))]
    (take-while #(not (identical? % eof))
                (repeatedly #(binding [*read-eval* false]
                               (clojure.core/read pbr false eof))))))