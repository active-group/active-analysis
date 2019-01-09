(ns active-analytics.clustering.k-medoids
  (:require [clojure.math.combinatorics :as combinatorics]
            [clojure.set :as clojure-set]))

(defn dissimilarity-map
  "Calculates a map of dissimilarities which is indexed by
  all combinations (pairs) of points."
  [xs distance-fn]
  (let [pairs (combinatorics/cartesian-product xs xs)]
    (loop [remaining pairs
           dissimilarities {}]
      (if (seq remaining)
        (let [[x y :as pair] (first remaining)
              dissimilarity (or (get dissimilarities [y x])
                                (distance-fn x y))]
          (recur (rest remaining)
                 (merge dissimilarities
                        {pair dissimilarity})))
        dissimilarities))))

(defn dissimilarity
  "Looks up the dissimilarity of two points
  `x` and `y` in a dissimilarity map."
  [x y dissimilarities]
  (get dissimilarities [x y]))

(defn dissimilarity-sum
  "Calculates the sum of dissimilarities of a point to
  all other points."
  [x xs dissimilarities]
  (- (apply +
          (map #(dissimilarity x % dissimilarities)
               xs))
     (dissimilarity x x dissimilarities)))

(defn choose-initial-medoids-randomly
  "Randomly chooses a set of medoids to start out with."
  [xs k]
  (->> xs
       shuffle
       (take k)
       (into #{})))

(defn first-pam-medoid
  "Determines the first medoid for the PAM BUILD process."
  [xs dissimilarities]
  (apply min-key
         #(dissimilarity-sum % xs dissimilarities)
         xs))

(defn min-medoid-distance
  "Calculates the distance to the closest medoid."
  [x medoids dissimilarities]
  (->> medoids
       (map #(dissimilarity x % dissimilarities))
       (apply min)))

(defn contribution
  "Calculates the contribution of `x` to the change in gain
  when adding `candidate` as additional medoid."
  [candidate x medoids dissimilarities]
  (max 0
       (- (min-medoid-distance x medoids dissimilarities)
          (dissimilarity candidate x dissimilarities))))

(defn gain
  "Calculates the gain associated with the choice
  of a medoid candidate."
  [candidate xs medoids dissimilarities]
  (->> xs
       (remove (partial = candidate))
       (map #(contribution candidate % medoids dissimilarities))
       (apply +)))

(defn next-pam-medoid
  "Determines the best additional medoid, that is,
  the point with maximal gain w.r.t. the currently
  chosen medoids."
  [xs medoids dissimilarities]
  (apply max-key
         #(gain % xs medoids dissimilarities)
         xs))

(defn pam-build
  "Performs the PAM BUILD initialization. That is,
  this greedily finds an initial set of medoids by
  iteratively choosing points which minimize dissimilarity."
  [xs dissimilarities k]
  (let [first-medoid (first-pam-medoid xs dissimilarities)]
    (loop [medoids #{first-medoid}]
      (if (= (count medoids)
             k)
        medoids
        (recur (conj medoids
                     (next-pam-medoid xs medoids dissimilarities)))))))

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

(defn find-medoid
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
          (map #(find-medoid % dissimilarities)
               (vals clusters)))))

(defn k-medoids
  "Performs k-medoid clustering on the data points `xs`,
  given the desired number of clusters `k` and a function
  `distance-fn` that computes the distance between two
  points.

  Accepts the keyword argument `:medoid-init-mode` which
  determines the way the initial set of medoids is created.
  The default value, `:random`, corresponds to random choice
  of initial medoids, while `:pam` utilizes the PAM BUILD
  algorithm to greedily choose `k` 'good' starting medoids."
  [xs distance-fn k & {:keys [medoid-init-mode] :or {medoid-init-mode :random}}]
  (let [dissimilarities (dissimilarity-map xs distance-fn)
        initial-medoids (case medoid-init-mode
                          :random (choose-initial-medoids-randomly xs k)
                          :pam (pam-build xs dissimilarities k))]
    (loop [medoids initial-medoids]
      (let [next-medoids (step xs medoids dissimilarities)]
        (if (= medoids
               next-medoids)
          {:clusters (vals (cluster xs medoids dissimilarities))}
          (recur next-medoids))))))
