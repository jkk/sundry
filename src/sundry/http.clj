(ns sundry.http
  (:require [clojure.string :as string])
  (:import [java.text SimpleDateFormat]
           [java.util Date TimeZone]))

(defn url-encode [s]
  (-> (java.net.URLEncoder/encode s "UTF-8")
      (.replace "+" "%20")
      (.replace "*" "%2A")
      (.replace "%7E" "~")))

(defn url-encode-path [path]
  (let [path (if (sequential? path)
               (string/join "/" path)
               path)
        parts (string/split path #"/")
        parts (if (re-find #"/$" path) ;preserve trailing slash
                (conj parts "")
                parts)]
    (string/join "/" (map url-encode parts))))

(defn url-decode [s]
  (when s
    (java.net.URLDecoder/decode s "UTF-8")))

;; TODO: handle nested values
(defn map->query-string [m]
  "Turns a map into a query string, stringifying and encoding as necessary"
  (string/join
   "&" (for [[k v] m]
         (if (or (set? v) (sequential? v))
           (string/join
             "&" (for [v* v]
                   (str (url-encode (name k)) "[]="
                        (url-encode (if (keyword? v*)
                                      (name v*)
                                      (str v*))))))
           (str (url-encode (name k)) "="
                (url-encode (if (keyword? v)
                              (name v)
                              (str v))))))))

;; TODO: handle nested and repeated params
(defn parse-query-string [qs]
  (reduce
   (fn [params pair]
     (if-let [[_ k v] (re-matches #"([^=]+)=(.*)" pair)]
       (assoc params (keyword (url-decode k)) (url-decode (or v "")))
       params))
   {}
   (.split qs "&")))

(defn max-age->expires [max-age]
  (let [df (doto (SimpleDateFormat. "EEE, dd-MMM-yyyy kk:mm:ss z")
             (.setTimeZone (TimeZone/getTimeZone "GMT")))]
    (.format df (Date. (+ (System/currentTimeMillis)
                          (* max-age 1000))))))

(defn- prep-params [params]
  (when params
    (str "?" (if (string? params)
               params
               (map->query-string params)))))

(defn make-href-fn [base-path]
  (fn [path & [params]]
    (str base-path
         (url-encode-path path)
         (prep-params params))))

(defn make-full-href-fn
  [{:keys [protocol host port path] :or {port 80}}]
  (fn [href-path & [params]]
    (str protocol "://"
         host
         (when (not= 80 port)
           (str ":" port))
         path
         (url-encode-path href-path)
         (prep-params params))))

(defn parse-url [url]
  (try
    (let [jurl (java.net.URL. url)]
      {:scheme (.getProtocol jurl)
       :user-info (.getUserInfo jurl)
       :host (.getHost jurl)
       :port (let [port (.getPort jurl)]
               (if (neg? port) nil port))
       :authority (.getAuthority jurl)
       :path (.getPath jurl)
       :query-string (.getQuery jurl)
       :fragment (.getRef jurl)
       :file (.getFile jurl)})
    (catch Exception _)))
