(ns sundry.ring
  (:require [sundry.http :refer [max-age->expires parse-url]]
            [sundry.ring.stateful-session :as sess]
            [sundry.jvm :refer [stacktrace-str]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.nested-params :refer [wrap-nested-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            ring.middleware.session
            ring.middleware.session.cookie
            [clojure.string :as string]))

(def wrap-all-params (comp wrap-multipart-params
                           wrap-params
                           wrap-nested-params
                           wrap-keyword-params))

(defn wrap-if [handler pred wrapper & args]
  (if pred
    (apply wrapper handler args)
    handler))

(defn- default-logger [req e]
  (locking System/out
    (println (format "Exception:\n%s" (stacktrace-str e)))))

(defn wrap-exception-logging
  ([handler]
    (wrap-exception-logging handler default-logger))
  ([handler logger]
    (fn [req]
      (try
        (handler req)
        (catch Throwable e
          (logger req e)
          (throw e))))))

(defn wrap-failsafe
  ([handler]
    (wrap-failsafe handler (constantly "<h1>Oops</h1><p>Something went wrong.</p>")))
  ([handler body-renderer]
    (fn [req]
      (try
        (handler req)
        (catch Throwable e
          {:status 500
           :headers {"Content-Type" "text/html"}
           :body (body-renderer req)})))))

(defn wrap-stacktrace [handler]
  (fn [req]
    (try
      (handler req)
      (catch Throwable e
        {:status 500
         :headers {"Content-Type" "text/html"}
         :body (str "<h1>Oops</h1><p>Something went wrong.</p>"
                    "<pre>"
                    (stacktrace-str e)
                    "</pre>") }))))

(def ^:dynamic *req* nil)

(defn wrap-request [handler]
  (fn [req]
    (binding [*req* req]
      (handler req))))

(defn- format-cache-opts [opts]
  (string/join ", " (for [[k v] opts]
                      (cond
                        (true? v) (name k)
                        (false? v) nil
                        :else (str (name k) "=" v)))))

(defn wrap-cache-control
  "See http://www.mnot.net/cache_docs/#CACHE-CONTROL

  Adds Expires header when :max-age option provided."
  [handler opts]
  (let [cache-headers {"Cache-Control" (format-cache-opts opts)}]
    (fn [req]
      (when-let [resp (handler req)]
        (let [cache-headers (if (:max-age opts)
                              (assoc cache-headers
                                     "Expires" (max-age->expires
                                                 (:max-age opts))))]
          (update-in resp [:headers] merge cache-headers))))))

(defn wrap-ajax-detect [handler]
  (fn [req]
    (handler (assoc req
               :ajax (= "XMLHttpRequest"
                        (get-in req [:headers "x-requested-with"]))))))

(def ^:dynamic *start-time* nil)

(defn wrap-timer [handler]
  (fn [req]
    (binding [*start-time* (System/nanoTime)]
      (handler req))))

(defn elapsed-ms []
  (/ (double (- (System/nanoTime) *start-time*)) 1000000.0))


(defn make-cookie-session-wrapper
  [& {:keys [path key cookie-name timeout]}]
  (let [sess-opts {:store (if key
                            (ring.middleware.session.cookie/cookie-store
                              {:key key})
                            (ring.middleware.session.cookie/cookie-store))}
        sess-opts (if path
                    (assoc sess-opts :root path)
                    sess-opts)
        sess-opts (if cookie-name
                    (assoc sess-opts :cookie-name cookie-name)
                    sess-opts)]
    (fn [handler]
      (fn [req]
        (let [sess-opts (if timeout
                          (assoc-in sess-opts [:cookie-attrs :expires]
                                    (max-age->expires timeout))
                          sess-opts)]
          ((ring.middleware.session/wrap-session handler sess-opts)
            req))))))

(def wrap-stateful-session sess/wrap-stateful-session)

(defn wrap-zd-proxy [handler]
  (fn [req]
    (if-let [proxy-url (get-in req [:headers "x-zd-url"])]
      (let [url (parse-url proxy-url)
            req* (assoc req
                        :scheme (:scheme url)
                        :server-name (:host url)
                        :server-port (or (:port url) 80)
                        :uri (:path url)
                        :headers (assoc (:headers req)
                                        "host" (:authority url)))]
        (handler req*))
      (handler req))))