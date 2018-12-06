(ns active-analytics.clustering.pic
  (:require [uncomplicate.neanderthal.core :refer :all]
            [uncomplicate.neanderthal.native :refer :all]))

(defn iterate
  "Given a (normalized) affinity matrix and a vector `v`,
  calculate the next vector by using power iteration."
  [affinity-matrix v]
  (let [v-new (mv affinity-matrix v)
        norm (nrm1 v-new)]
    (scal (/ 1 norm) v-new)))

(defn pic
  "Performs power iteration clustering with an `affinity-matrix`,
  the desired number of clusters `k`, and a number of iterations
  and threshold which determine when to terminate the algorithm."
  [affinity-matrix k v-0 max-number-of-iterations threshold]
  (loop [i 0
         v v-0]
    (let [v-new (iterate affinity-matrix v)]
      (if (or (>= i max-number-of-iterations)
              (<= (nrm1 (axpy v v-new)) threshold))
        v-new
        (recur (inc i) v-new)))))
