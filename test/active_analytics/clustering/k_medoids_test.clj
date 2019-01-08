(ns active-analytics.clustering.k-medoids-test
  (:require [active-analytics.clustering.k-medoids :as k-medoids]
            [clojure.test :refer [deftest is testing]]
            [active-analytics.test-util :as test-util]))

(deftest t-dissimilarity
  (let [dissimilarities (k-medoids/dissimilarity-map [1 3 7 -4] test-util/real-distance)]
    (is (= 2.0
           (k-medoids/dissimilarity 3 1 dissimilarities)))
    (is (= 2.0
           (k-medoids/dissimilarity 1 3 dissimilarities)))
    (is (= 11.0
           (k-medoids/dissimilarity -4 7 dissimilarities)))))

(deftest t-dissimilarity-sum
  (let [xs [1 2 -3 7]
        dissimilarities (k-medoids/dissimilarity-map xs test-util/real-distance)]
    (is (= 11.0
           (k-medoids/dissimilarity-sum 1 xs dissimilarities)))
    (is (= 21.0
           (k-medoids/dissimilarity-sum 7 xs dissimilarities)))))

(deftest t-nearest-medoid
  (let [dissimilarities (k-medoids/dissimilarity-map [-1 2 -6 13] test-util/real-distance)]
    (is (= -6
           (k-medoids/nearest-medoid -6 #{-6 -1} dissimilarities)))
    (is (= 2
           (k-medoids/nearest-medoid 13 #{-1 2} dissimilarities)))))

(deftest t-cluster
  (let [xs [-11 4 5 -1 3]
        dissimilarities (k-medoids/dissimilarity-map xs test-util/real-distance)]
    (is (= {-1 [-11 -1] 4 [4 5 3]}
           (k-medoids/cluster xs #{-1 4} dissimilarities)))))

(deftest t-choose-medoid)
(let [dissimilarities (k-medoids/dissimilarity-map [-11 4 5 -1 3] test-util/real-distance)]
  (is (= 3
         (k-medoids/choose-medoid [4 5 3 -1] dissimilarities))))

(deftest t-k-medoids
  (let [xs [-6 11 0 -3 14 13]
        result (k-medoids/k-medoids xs test-util/real-distance 2)]
    (is (= #{[-6 0 -3]
             [11 14 13]}
           (into #{}
                 (:clusters result))))))
