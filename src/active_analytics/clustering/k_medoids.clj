(ns active-analytics.clustering.k-medoids
  (:require [clojure.math.combinatorics :as combinatorics]
            [clojure.set :as clojure-set]))

(defn dissimilarity-map
  "Calculates a map of dissimilarities which is indexed by
  unique combinations (pairs) of points."
  [xs distance-fn]
  (let [unique-pairs (combinatorics/combinations xs 2)]
    (into {}
          (map (fn [p]
                 [p (apply distance-fn p)])
               unique-pairs))))

(defn dissimilarity
  "Looks up the dissimilarity of two points
  `x` and `y` in a dissimilarity map."
  [x y dissimilarities]
  (or (get dissimilarities [x y])
      (get dissimilarities [y x])
      0))

(defn dissimilarity-sum
  "Calculates the sum of dissimilarities of a point to
  all other points.
  Assumes that the point has dissimilarity 0 to itself."
  [x xs dissimilarities]
  (apply +
         (map #(dissimilarity x % dissimilarities)
              xs)))

(defn choose-initial-medoids
  "Randomly chooses a set of medoids to start out with."
  [xs k]
  (->> xs
       shuffle
       (take k)
       (into #{})))

(defn nearest-medoid
  "Finds the medoid that is closest to a point `x`."
  [x medoids dissimilarities]
  (apply min-key
         #(dissimilarity x % dissimilarities)
         medoids))

(defn cluster
  "Groups the data points `xs` by assigning each to its
  nearest medoid."
  [xs medoids dissimilarities]
  (group-by #(nearest-medoid % medoids dissimilarities)
            xs))

(defn choose-medoid
  "Finds the medoid of a set of data points `xs`, that is,
  the point with the lowest sum of dissimilarities."
  [xs dissimilarities]
  (apply min-key
         #(dissimilarity-sum % xs dissimilarities)
         xs))

(defn step
  "Calculates the next generation of medoids from the current one."
  [xs medoids dissimilarities]
  (let [clusters (cluster xs medoids dissimilarities)]
    (into #{}
          (map #(choose-medoid % dissimilarities)
               (vals clusters)))))

(defn k-medoids
  "Performs k-medoid clustering on the data points `xs`,
  given the desired number of clusters `k` and a function
  `distance-fn` that computes the distance between two
  points."
  [xs distance-fn k]
  (let [dissimilarities (dissimilarity-map xs distance-fn)]
    (loop [medoids (choose-initial-medoids xs k)]
      (let [next-medoids (step xs medoids dissimilarities)]
        (if (= medoids
               next-medoids)
          {:clusters (vals (cluster xs medoids dissimilarities))}
          (recur next-medoids))))))
