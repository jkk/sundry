(ns util.http
  (:require [clojure.string :as string])
  (:import [java.text SimpleDateFormat]
           [java.util Date TimeZone]))

(defn url-encode [s]
  (-> (java.net.URLEncoder/encode s "UTF-8")
      (.replace "+" "%20")
      (.replace "*" "%2A")
      (.replace "%7E" "~")))

(defn url-encode-path [path]
  (let [parts (string/split path #"/")
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
         (str (url-encode (name k)) "="
              (url-encode (if (keyword? v)
                            (name v)
                            (str v)))))))

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
