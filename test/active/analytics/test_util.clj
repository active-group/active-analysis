(ns active.analytics.test-util
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn close?
  "Tests whether two numbers `x` and `y` are within a
  certain distance `epsilon` from each other."
  [x y epsilon]
  (<= (Math/abs (- x y))
      epsilon))

(deftest t-close?
  (is (close? 3.1 3.1 1e-5))
  (is (close? 5 4 1))
  (is (not (close? 10 11 0.9)))
  (is (close? 1e-27 1e-28 1e-27))
  (is (close? -2 2 10))
  (is (not (close? 0 0.1 0.05))))

(def ^:dynamic *quickcheck-size* 100)

(defmacro is-quickcheck
  "Wraps `clojure.test.check/quick-check` to be able to
  use it with `clojure.test` and multiple 'testing' blocks."
  [property]
  `(let [result# (check/quick-check ~*quickcheck-size*
                                    ~property)]
     (or (clojure.test/is (:result result#))
         (clojure.test/is (= nil result#)))))

(defn random-data-generator
  "Returns a generator that generates a Clojure vector of
  random length between 2 and `max-number-of-data-points`,
  containing double vectors of a random dimension between
  2 and `max-dimension`."
  [max-dimension max-number-of-data-points]
  (gen/bind (gen/choose 2 max-dimension)
            (fn [dim]
              (gen/vector-distinct (gen/vector (gen/double* {:infinite? false
                                                             :NaN? false
                                                             :min -10000000
                                                             :max 10000000})
                                               dim)
                                   {:min-elements 2
                                    :max-elements max-number-of-data-points}))))

(defn circular-cluster-generator
  "Returns a generator that generates `number-of-data-points`
  data points randomly in a ball with radius `radius`
  (w.r.t. the 1-norm) around `center`."
  [center radius number-of-data-points]
  (let [dimension (count center)]
    (gen/vector (gen/bind
                 (gen/vector (gen/double* {:NaN? false
                                           :min (- radius)
                                           :max radius})
                             dimension)
                 (fn [v]
                   (gen/elements [(mapv + center v)])))
                number-of-data-points)))

(defn p-distance
  "Gets a function that calculates the distance between
  two vectors w.r.t. the `p`-norm."
  [p]
  (fn [v w]
    (let [d (map #(Math/pow (Math/abs (- %1 %2)) p)
                 v
                 w)]
      (Math/pow (apply + d)
                (/ 1 p)))))
