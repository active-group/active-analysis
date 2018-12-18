(ns active-analytics.clustering.k-means-test
  (:require [active-analytics.clustering.k-means :as k-means]
            [active-analytics.test-util :as test-util]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest t-choose-initial-centroids
  (testing "All chosen centroids should be in data"
    (test-util/is-quickcheck
     (prop/for-all [data (gen/vector gen/large-integer 5 100)
                    k (gen/choose 1 5)]
                   (every? (fn [centroid]
                             (some (partial = centroid)
                                   data))
                           (k-means/choose-initial-centroids data k))))))

(deftest t-cluster
  (let [data [1 2 3 4 5 6]
        centroids [1 5 100]]
    (is (= {1 [1 2]
            5 [3 4 5 6]
            100 nil}
           (k-means/cluster data
                            centroids
                            test-util/real-distance)))))

(deftest t-nearest-centroid
  (is (= -2
         (k-means/nearest-centroid 2
                                   [-5 -2 7 22]
                                   test-util/real-distance))))

(defn average
  [vecs]
  (let [n (count vecs)]
    (apply mapv
           (fn [& xs]
             (/ (apply + xs)
                n))
           vecs)))

(deftest t-k-means
  (testing "k-means needs either `:k` or `:initial-centroids`"
    (is (thrown? Exception
                 (k-means/k-means [1 2 3]
                                  test-util/real-distance
                                  5
                                  :centroid-fn #(/ (apply + %) (count %))
                                  :threshold 1e-5))))
  (testing "There should be at most `k` clusters"
    (is (every? true?
                (for [i (range test-util/*quickcheck-size*)]
                  (let [size-1 (gen/generate (gen/choose 2 50) i)
                        data (gen/generate (gen/let [size-2 (gen/choose 2 50)
                                                     cluster-1 (test-util/circular-cluster-generator [-10 -10 -3] 2.4 size-1)
                                                     cluster-2 (test-util/circular-cluster-generator [5 3 7] 1.91 size-2)]
                                             (concat cluster-1 cluster-2))
                                           i)
                        k-means-result (k-means/k-means data
                                                        (test-util/p-distance 2)
                                                        10
                                                        :k 2
                                                        :threshold 1e-5
                                                        :centroid-fn average)]
                    (<= (count (:clusters k-means-result))
                        2))))))
  (testing "Without a `threshold`, the maximum number of iterations will be performed"
    (let [result (k-means/k-means [1 2 3 4 5 101 102 103 104 105]
                                  test-util/real-distance
                                  200
                                  :k 2
                                  :centroid-fn #(/ (apply + %) (count %)))]
      (is (= 200
             (:iterations result)))))
  (testing "Algorithm may finish earlier when `threshold` is given"
    (let [result (k-means/k-means [1 2 3 4 5 101 102 103 104 105]
                                  test-util/real-distance
                                  50
                                  :threshold 1e-2
                                  :initial-centroids [0 100]
                                  :centroid-fn #(/ (apply + %) (count %)))]
      (is (< (:iterations result)
             10)))))
