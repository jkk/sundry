(ns sundry.ns)

(defmacro defalias [sym var-sym]
  `(let [v# (var ~var-sym)]
     (intern *ns* (with-meta (quote ~sym) (meta v#)) @v#)))