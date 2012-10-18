(ns sundry.ring.stateful-session
  (:refer-clojure :exclude [get]))

(def ^:dynamic *session* (atom nil))

(defn wrap-stateful-session [handler]
  (fn [req]
    (let [initial-session (:session req)]
      (binding [*session* (atom initial-session)]
        (let [resp (handler req)
              new-session (and (or (:session resp) @*session*)
                               (merge (:session resp) @*session*))]
          (if (= initial-session new-session)
            resp
            (assoc resp :session new-session)))))))

(defn update! [f & args]
  (apply swap! *session* f args))

(defn put! [& kvs]
  (apply update! assoc kvs))

(defn get
  ([k]
     (get k nil))
  ([k default]
     (clojure.core/get @*session* k default)))

(defn get-all []
  @*session*)
