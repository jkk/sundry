(ns sundry.io-test
  (:require
   [clojure.test :refer [deftest is]]
   [sundry.io :as io])
  (:import java.io.StringReader))

(deftest test-read
  (let [data [:foo #{"bar" {1 'baz 2 (java.util.Date.)}}]]
    (is (= data (io/read (StringReader. (pr-str data)))))
    (is (thrown? Throwable (io/read (StringReader. "[:foo #=(+ 1 1)]"))))))

(deftest test-read-seq
  (is (= [:foo :bar] (io/read-seq (StringReader. ":foo :bar"))))
  ;;(is (thrown? Throwable (io/read-seq (StringReader. ":foo [:bar #=(+ 1 1)]"))))
  )