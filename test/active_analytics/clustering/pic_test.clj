(ns active-analytics.clustering.pic-test
  (:require [active-analytics.clustering.pic :refer :all]
            [clojure.test :refer [deftest is testing]]
            [uncomplicate.neanderthal.core :refer :all]
            [uncomplicate.neanderthal.native :refer :all]))

(def points
  (map dv
       [[0 1]
        [1 0]
        [1.1 0]
        [-0.1 0.9]
        [2 0.4]]))

(def m
  (dge 5 5
       (mapcat (fn [p]
                 (map (fn [q]
                        (nrm1 (axpy -1 p q)))
                      points))
               points)))

(pic m 1 (dv [1 1 1 1 1]) 100 0.0001)
