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
           (k-medoids/dissimilarity -4 7 dissimilarities)))
    (is (= 0.0
           (k-medoids/dissimilarity 7 7 dissimilarities)))))

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

(deftest t-find-medoid
  (let [dissimilarities (k-medoids/dissimilarity-map [-11 4 5 -1 3] test-util/real-distance)]
    (is (= 3
           (k-medoids/find-medoid [4 5 3 -1] dissimilarities)))))

(deftest t-k-medoids
  (let [xs [-6 11 0 -3 14 13]
        result (k-medoids/k-medoids xs test-util/real-distance 2)]
    (is (= #{[-6 0 -3]
             [11 14 13]}
           (into #{}
                 (:clusters result)))))
  (let [xs [-6 11 0 -3 14 13]
        result (k-medoids/k-medoids xs test-util/real-distance 2 :medoid-init-mode :pam)]
    (is (= #{[-6 0 -3]
             [11 14 13]}
           (into #{}
                 (:clusters result))))))

(deftest t-first-pam-medoid
  (let [xs [-8 -5 0 2 9]
        dissimilarities (k-medoids/dissimilarity-map xs test-util/real-distance)]
    (is (= 0
           (k-medoids/first-pam-medoid xs dissimilarities)))))

(deftest t-min-medoid-distance
  (let [xs [[1 4] [-3 1] [0 1] [2 0] [5 17] [3 3] [1 1]]
        dissimilarities (k-medoids/dissimilarity-map xs (test-util/p-distance 1))]
    (is (= 1.0
           (k-medoids/min-medoid-distance [1 1]
                                          [[-3 1] [3 3] [0 1] [2 0]]
                                          dissimilarities)))))

(deftest t-contribution
  (let [xs [6 -3 5 0 1 12 -5 2]
        dissimilarities (k-medoids/dissimilarity-map xs test-util/real-distance)]
    (is (= 0
           (k-medoids/contribution -5 2 #{-3 5} dissimilarities)))
    (is (= 2.0
           (k-medoids/contribution -5 -3 #{1 2} dissimilarities)))))

(deftest t-gain
  (let [xs [6 -3 5 0 1 12 -5 2]
        dissimilarities (k-medoids/dissimilarity-map xs test-util/real-distance)]
    (is (= 2.0
           (k-medoids/gain -5 xs #{1} dissimilarities)))
    (is (= 6.0
           (k-medoids/gain 2 xs #{0} dissimilarities)))
    (is (= 6.0
           (k-medoids/gain 5 xs #{-3 2} dissimilarities)))))

(deftest t-next-pam-medoid
  (let [xs [6 -3 5 0 1 12 -5 2]
        dissimilarities (k-medoids/dissimilarity-map xs test-util/real-distance)]
    (is (= 5
           (k-medoids/next-pam-medoid xs #{2} dissimilarities))))
  (let [xs [-5 -2 0 3 10]
        dissimilarities (k-medoids/dissimilarity-map xs test-util/real-distance)]
    (is (= 3
           (k-medoids/next-pam-medoid xs #{-2} dissimilarities)))))

(deftest t-pam-build
  (let [xs [6 -3 5 0 1 12 -5 2]
        dissimilarities (k-medoids/dissimilarity-map xs test-util/real-distance)]
    (is (= #{2}
           (k-medoids/pam-build xs dissimilarities 1)))
    (is (= #{2 5}
           (k-medoids/pam-build xs dissimilarities 2)))
    (is (= #{2 5 -3}
           (k-medoids/pam-build xs dissimilarities 3)))
    (is (= #{2 5 -3 1 6}
           (k-medoids/pam-build xs dissimilarities 5)))))
