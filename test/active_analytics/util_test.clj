(ns active-analytics.util-test
  (:require [active-analytics.util :as util]
            [clojure.test :refer [deftest testing is]]))

(deftest t-average-or-zero
  (testing "Empty seq yields 0"
    (is (= 0
           (util/average-or-zero []))))
  (testing "Average is calculated for non-empty seqs"
    (is (= 3
           (util/average-or-zero '(0 1 4 7))))))
