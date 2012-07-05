(ns util.read-test
  (:require
   [clojure.test :refer [deftest is]]
   [util.read :as r])
  (:import java.io.StringReader))

(deftest test-read
  (let [data [:foo #{"bar" {1 'baz 2 (java.util.Date.)}}]]
    (is (= data (r/read (StringReader. (pr-str data)))))
    (is (thrown? Throwable (r/read (StringReader. "[:foo #=(+ 1 1)]"))))))

(deftest test-read-seq
  (is (= [:foo :bar] (r/read-seq (StringReader. ":foo :bar"))))
  ;;(is (thrown? Throwable (r/read-seq (StringReader. ":foo [:bar #=(+ 1 1)]"))))
  )