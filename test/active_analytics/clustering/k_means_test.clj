(ns active-analytics.clustering.k-means-test
  (:require [active-analytics.clustering.k-means :as k-means]
            [clojure.test :refer [deftest is testing]]
            [uncomplicate.neanderthal.core :as neanderthal]
            [uncomplicate.neanderthal.native :as native]))

(def points
  (map #(neanderthal/vctr native/native-double [%])
       [-3.2 -4 -3.1 0 1 0.5 5 6 7 8 5.7]))

(def initial-centroids
  (map #(neanderthal/vctr native/native-double [%])
       [1 2 3]))

(k-means/k-means points
                 3
                 100
                 (comp neanderthal/nrm1 (partial neanderthal/axpy -1))
                 initial-centroids)
