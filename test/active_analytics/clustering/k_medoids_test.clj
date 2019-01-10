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
        dissimilarity-fn (k-medoids/dissimilarity-function xs test-util/real-distance true)]
    (is (= 11.0
           (k-medoids/dissimilarity-sum 1 xs dissimilarity-fn)))
    (is (= 21.0
           (k-medoids/dissimilarity-sum 7 xs dissimilarity-fn)))))

(deftest t-nearest-medoid
  (let [dissimilarity-fn (k-medoids/dissimilarity-function [-1 2 -6 13] test-util/real-distance true)]
    (is (= -6
           (k-medoids/nearest-medoid -6 #{-6 -1} dissimilarity-fn)))
    (is (= 2
           (k-medoids/nearest-medoid 13 #{-1 2} dissimilarity-fn)))))

(deftest t-cluster
  (let [xs [-11 4 5 -1 3]
        dissimilarity-fn (k-medoids/dissimilarity-function xs test-util/real-distance true)]
    (is (= {-1 [-11 -1] 4 [4 5 3]}
           (k-medoids/cluster xs #{-1 4} dissimilarity-fn)))))

(deftest t-find-medoid
  (let [dissimilarity-fn (k-medoids/dissimilarity-function [-11 4 5 -1 3] test-util/real-distance true)]
    (is (= 3
           (k-medoids/find-medoid [4 5 3 -1] dissimilarity-fn)))))

(deftest t-k-medoids
  (let [xs [-6 11 0 -3 14 13]]
    (testing "Asserts that `cache?` is boolean"
      (is (thrown? AssertionError
                   (k-medoids/k-medoids xs test-util/real-distance 2 :cache? 3))))
    (testing "Asserts that `medoid-init-mode` is a known value"
      (is (thrown? AssertionError
                   (k-medoids/k-medoids xs test-util/real-distance 3 :medoid-init-mode :foo))))
    (let [result (k-medoids/k-medoids xs test-util/real-distance 2)]
      (is (= #{[-6 0 -3]
               [11 14 13]}
             (into #{}
                   (:clusters result)))))
    (let [result (k-medoids/k-medoids xs test-util/real-distance 2 :medoid-init-mode :pam)]
      (is (= #{[-6 0 -3]
               [11 14 13]}
             (into #{}
                   (:clusters result)))))))

(deftest t-first-pam-medoid
  (let [xs [-8 -5 0 2 9]
        dissimilarity-fn (k-medoids/dissimilarity-function xs test-util/real-distance true)]
    (is (= 0
           (k-medoids/first-pam-medoid xs dissimilarity-fn)))))

(deftest t-min-medoid-distance
  (let [xs [[1 4] [-3 1] [0 1] [2 0] [5 17] [3 3] [1 1]]
        dissimilarity-fn (k-medoids/dissimilarity-function xs (test-util/p-distance 1) true)]
    (is (= 1.0
           (k-medoids/min-medoid-distance [1 1]
                                          [[-3 1] [3 3] [0 1] [2 0]]
                                          dissimilarity-fn)))))

(deftest t-contribution
  (let [xs [6 -3 5 0 1 12 -5 2]
        dissimilarity-fn (k-medoids/dissimilarity-function xs test-util/real-distance true)]
    (is (= 0
           (k-medoids/contribution -5 2 #{-3 5} dissimilarity-fn)))
    (is (= 2.0
           (k-medoids/contribution -5 -3 #{1 2} dissimilarity-fn)))))

(deftest t-gain
  (let [xs [6 -3 5 0 1 12 -5 2]
        dissimilarity-fn (k-medoids/dissimilarity-function xs test-util/real-distance true)]
    (is (= 2.0
           (k-medoids/gain -5 xs #{1} dissimilarity-fn)))
    (is (= 6.0
           (k-medoids/gain 2 xs #{0} dissimilarity-fn)))
    (is (= 6.0
           (k-medoids/gain 5 xs #{-3 2} dissimilarity-fn)))))

(deftest t-next-pam-medoid
  (let [xs [6 -3 5 0 1 12 -5 2]
        dissimilarity-fn (k-medoids/dissimilarity-function xs test-util/real-distance true)]
    (is (= 5
           (k-medoids/next-pam-medoid xs #{2} dissimilarity-fn))))
  (let [xs [-5 -2 0 3 10]
        dissimilarity-fn (k-medoids/dissimilarity-function xs test-util/real-distance true)]
    (is (= 3
           (k-medoids/next-pam-medoid xs #{-2} dissimilarity-fn)))))

(deftest t-pam-build
  (let [xs [6 -3 5 0 1 12 -5 2]
        dissimilarity-fn (k-medoids/dissimilarity-function xs test-util/real-distance true)]
    (is (= #{2}
           (k-medoids/pam-build xs dissimilarity-fn 1)))
    (is (= #{2 5}
           (k-medoids/pam-build xs dissimilarity-fn 2)))
    (is (= #{2 5 -3}
           (k-medoids/pam-build xs dissimilarity-fn 3)))
    (is (= #{2 5 -3 1 6}
           (k-medoids/pam-build xs dissimilarity-fn 5)))))
