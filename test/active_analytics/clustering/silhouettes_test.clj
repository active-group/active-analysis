(ns active-analytics.clustering.silhouettes-test
  (:require [active-analytics.clustering.silhouettes :as silhouettes]
            [clojure.test :refer [deftest testing is]]
            [active-analytics.test-util :as test-util]
            [active-analytics.util :as util]))

(deftest t-average-distance
  (is (= 5.0
         (silhouettes/average-distance 5 [-1 0 1 5] test-util/real-distance)))
  (is (= 1.0
         (silhouettes/average-distance [1 2] [[1 3] [1 2] [2 2]] (test-util/p-distance 1))))
  (is (= 4.0
         (silhouettes/average-distance -2 [5 -3 2] test-util/real-distance)))
  (is (= 0
         (silhouettes/average-distance -1 [-1] test-util/real-distance))))

(deftest t-average-distance-to-closest-cluster
  (is (= 4.0
         (silhouettes/average-distance-to-closest-cluster 5 [[2 -2 3] [10 12]] test-util/real-distance))))

(deftest t-silhouette-width
  (is (= 0.75
         (silhouettes/silhouette-width 5 [4 5 6] [[2 -2 3] [10 12]] test-util/real-distance))))

(deftest t-average-silhouette-width
  (is (= 0.65
         (silhouettes/average-silhouette-width [4 5 6] [[2 -2 3] [10 12]] test-util/real-distance))))

(deftest t-total-average-silhouette-width
  (is (test-util/close? (util/average-or-zero [0.0634920634920635
                                               0.65
                                               0.6571428571428571])
                        (silhouettes/total-average-silhouette-width [[2 -2 3] [4 5 6] [10 12]]
                                                                    test-util/real-distance)
                        1e-5)))
