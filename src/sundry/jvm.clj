(ns sundry.jvm
  (:require [clojure.repl :as repl]))

(defn mem-stats
  "Return stats about memory availability and usage, in MB. Calls
  System/gc before gathering stats when the :gc option is true."
  [& {:keys [gc]}]
  (when gc
    (System/gc))
  (let [r (Runtime/getRuntime)
        mb #(int (/ % 1024 1024))]
    {:max (mb (.maxMemory r))
     :total (mb (.totalMemory r))
     :used (mb (- (.totalMemory r) (.freeMemory r)))
     :free (mb (.freeMemory r))}))

(defn stacktrace-str [e]
  (let [sw (java.io.StringWriter.)]
    (binding [*err* sw]
      (repl/pst e 1000)
      (str sw))))