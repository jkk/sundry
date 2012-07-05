(ns util.seq-test
  (:require
   [clojure.test :refer [deftest is]]
   [util.seq :refer :all]))

(deftest test-interleave-all
  (is (= (interleave [1 2 3] [4 5 6]) (interleave-all [1 2 3] [4 5 6])))
  (is (= [1 5 2 6 3 4] (interleave-all [1 2 3 4] [5 6]))))
