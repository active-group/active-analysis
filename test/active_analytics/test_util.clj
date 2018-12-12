(ns active-analytics.test-util
  (:require [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn close?
  "Tests whether two numbers `x` and `y` are within a
  certain distance `epsilon` from each other."
  [x y epsilon]
  (< (Math/abs (- x y))
     epsilon))

(def ^:dynamic *quickcheck-size* 100)

(defmacro is-quickcheck
  "Wraps `clojure.test.check/quick-check` to be able to
  use it with `clojure.test` and multiple 'testing' blocks."
  [property]
  `(let [result# (check/quick-check ~*quickcheck-size*
                                   ~property)]
     (or (clojure.test/is (:result result#))
         (clojure.test/is (= nil result#)))))

(macroexpand-1 '(is-quickcheck (prop/for-all [v gen/double]
                                             (= 1 v))))

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
                                    :max-elemets max-number-of-data-points}))))

(defn circular-cluster-generator
  "Returns a generator that generates `number-of-data-points`
  data points in dimension `dimension` randomly in a ball
  around `center` with a given `radius` w.r.t. the 1-norm."
  [dimension center radius number-of-data-points]
  (gen/vector (gen/bind
               (gen/vector (gen/double* {:NaN? false
                                         :min (- radius)
                                         :max radius})
                           dimension)
               (fn [v]
                 (gen/elements [(mapv + center v)])))
              number-of-data-points))

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
