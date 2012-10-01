(ns sundry.string
  (:require [clojure.string :as string]))

(defn capitalize [s]
  (str (Character/toTitleCase (char (first s)))
       (subs s 1)))

(def ^:private title-case-exclusions #{"the" "an" "a" "in" "into"})

(defn title-case [s]
  (let [words (string/split s #"\s+")]
    (string/join " " (cons (capitalize (first words))
                           (for [word (rest words)]
                             (if (title-case-exclusions word)
                               word
                               (capitalize word)))))))

(defn remove-accents [^String s]
  (-> (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFD)
      (.replaceAll "[^\\p{ASCII}]" "")))
