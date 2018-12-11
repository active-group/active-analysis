(ns active-analytics.clustering.pic-test
  (:require [active-analytics.clustering.pic :as pic]
            [active-analytics.linear-algebra :as lina]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [uncomplicate.neanderthal.core :as neanderthal]))

(defn p-distance
  "Gets a function that calculates the distance between
  two vectors w.r.t. the p-norm."
  [p]
  (fn [v w]
    (let [d (map #(Math/pow (Math/abs (- %1 %2)) p)
                 v
                 w)]
      (Math/pow (apply + d)
                (/ 1 p)))))

(deftest t-create-affinity-matrix!
  (testing "Simple distance examples"
    (is (= (lina/ge [[0 0]
                     [0 0]])
           (pic/create-affinity-matrix! [2 2]
                                        #(Math/abs (- %1 %2)))))
    (is (= (lina/ge [[0 4 3]
                     [4 0 7]
                     [3 7 0]])
           (pic/create-affinity-matrix! [1 -3 4]
                                        #(Math/abs (- %1 %2))))))
  (testing "Distance matrix is symmetric"
    (is
     (true?
      (:result
       (check/quick-check 100
                          (prop/for-all [v (gen/bind (gen/large-integer* {:min 2 :max 5})
                                                     (fn [n]
                                                       (gen/vector
                                                        (gen/vector (gen/double* {:infinite? false
                                                                                  :NaN? false})
                                                                    n)
                                                        2
                                                        100)))
                                         p (gen/large-integer* {:min 1 :max 10})]
                                        (let [m (pic/create-affinity-matrix! v
                                                                             (p-distance p))]
                                          (= m
                                             (neanderthal/trans m))))))))))
