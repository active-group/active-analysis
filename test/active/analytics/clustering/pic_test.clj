(ns active.analytics.clustering.pic-test
  (:require [active.analytics.clustering.pic :as pic]
            [active.analytics.linear-algebra :as lina]
            [active.analytics.test-util :as test-util :refer [is-quickcheck]]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest t-create-affinity-matrix!
  (testing "Simple distance examples"
    (is (= (lina/ge [[0 0]
                     [0 0]])
           (pic/create-affinity-matrix! [2 2]
                                        #(Math/abs (- %1 %2)))))
    (is (= (lina/ge [[0 4 3]
                     [4 0 7]
                     [3 7 0]])
           (pic/create-affinity-matrix! [[1] [-3] [4]]
                                        (test-util/p-distance 1))))
    (is (= (lina/ge [[0 1 2]
                     [1 0 3]
                     [2 3 0]])
           (pic/create-affinity-matrix! [[0 1] [0 2] [0 -1]]
                                        (test-util/p-distance 2)))))
  (testing "Affinity matrix is symmetric"
    (is-quickcheck
     (prop/for-all [data (test-util/random-data-generator 10 50)
                    p (gen/choose 1 3)]
                   (lina/symmetric? (pic/create-affinity-matrix! data
                                                                 (test-util/p-distance p)))))))

(deftest t-create-normalized-affinity-matrix!
  (testing "A normalized affinity matrix should be normalized"
    (is-quickcheck
     (prop/for-all [data (test-util/random-data-generator 10 50)
                    p (gen/choose 1 3)]
                   (let [w (pic/create-normalized-affinity-matrix! data
                                                                   (test-util/p-distance p))
                         row-sums (map lina/sum (lina/rows w))]
                     (every? #(test-util/close? % 1 1e-5)
                             row-sums))))))

(deftest t-initial-vector
  (let [a (pic/create-affinity-matrix! [[1] [2] [3]]
                                       (test-util/p-distance 1))]
    (is (= (lina/vctr [0 1 2])
           (pic/initial-vector a)))
    (is (= (lina/vctr [0.375 0.25 0.375])
           (pic/initial-vector a :range? false)))))

(deftest t-scale
  (is (test-util/close? 1.7
                        (pic/scale (lina/vctr [-1.4 0.1 -0.4 0.3]))
                        1e-5)))

(deftest t-threshold
  (is (test-util/close? 1e-5
                        (pic/threshold 1)
                        1e-10))
  (is (test-util/close? 1e-7
                        (pic/threshold 100)
                        1e-10))
  (is (test-util/close? 1e-8
                        (pic/threshold 1e10)
                        1e-10)))

(deftest t-pic-only
  (is (every? true?
              (for [i (range test-util/*quickcheck-size*)]
                (let [size-1 (gen/generate (gen/choose 2 50) i)
                      data (gen/generate (gen/let [size-2 (gen/choose 2 50)
                                                   cluster-1 (test-util/circular-cluster-generator [0 -1 1] 2.4 size-1)
                                                   cluster-2 (test-util/circular-cluster-generator [5 3 -7] 1.91 size-2)]
                                           (concat cluster-1 cluster-2))
                                         i)
                      pic-result (pic/pic-only (pic/create-normalized-affinity-matrix! data
                                                                                       (test-util/p-distance 2))
                                               500)
                      left (take size-1 pic-result)
                      right (drop size-1 pic-result)
                      mean #(/ (apply + %)
                               (count %))]
                  ;; Sanity check: assert that no points 'jump' into the other cluster
                  (or (if (< (mean left)
                             (mean right))
                        (< (apply max left)
                           (apply min right))
                        (> (apply min left)
                           (apply max right)))
                      (println "Fail:" data)
                      (println "Clustered:" (map identity pic-result))))))))
