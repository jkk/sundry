(ns sundry.num-test
  (:require
   [clojure.test :refer [deftest is]]
   [sundry.num :refer :all]))

(deftest test-parse-int
  (is (= 123 (parse-int "123")))
  (is (nil? (parse-int "123.0")))
  (is (= 123 (parse-int "0123")))
  (is (= 123 (parse-int 123.0))))

(deftest test-parse-long
  (is (= 123 (parse-long "123")))
  (is (nil? (parse-long "123.0")))
  (is (= 123 (parse-long "0123")))
  (is (= 123 (parse-long 123.0))))

(deftest test-parse-double
  (is (= 123.0 (parse-double "123")))
  (is (= 123.0 (parse-double "123.0")))
  (is (= 123.0 (parse-double "0123.0")))
  (is (= 123.0 (parse-double 123))))
