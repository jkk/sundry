(ns sundry.io
  (:refer-clojure :exclude [read])
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string])
  (:import
    [java.io File]))

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

(defn gzip [^String str]
  (with-open [os (java.io.ByteArrayOutputStream.)]
    (let [gos (java.util.zip.GZIPOutputStream. os)]
      (.write gos (.getBytes str "UTF-8"))
      (.close gos)
      (.toByteArray os))))

(defn ungzip [bytes]
  (with-open [is (java.io.ByteArrayInputStream. bytes)
              gis (java.util.zip.GZIPInputStream. is)
              os (java.io.ByteArrayOutputStream.)]
    (io/copy gis os)
    (String. (.toByteArray os) "UTF-8")))

(defn gz-serialize [data]
  (-> data pr-str gzip))

(defn gz-unserialize [bytes]
  (-> bytes ungzip read-string))

(defmulti get-extension
  "Returns the file extension of the given object - e.g., of a String or File.
  The extension will be lowercased and normalized."
  (fn [f] (class f)))

(defmethod get-extension :default [f]
  (throw (IllegalArgumentException.
           (str "Could not determine extension of " f))))

(def ^:private extension-normalizations
  {"jpeg" "jpg"
   "jpe" "jpg"
   "tiff" "tif"})

(defn normalize-extension [ext]
  (let [ext* (string/lower-case ext)]
    (extension-normalizations ext* ext*)))

(defmethod get-extension String [f]
  (normalize-extension
    (second (re-find #"\.([^\.]+)$" f))))

(defmethod get-extension File [^File f]
  (get-extension (.getName f)))

