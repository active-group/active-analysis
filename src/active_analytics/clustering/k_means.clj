(ns active-analytics.clustering.k-means
  (:require [active-analytics.linear-algebra :as lina]))

(defn choose-initial-centroids
  "Takes `k` random (distinct) elements from `data` to use
  as initial centroids."
  [data k]
  (->> data
       shuffle
       (take k)))

(defn nearest-centroid
  "Finds the centroid closest to a point `p`."
  [p centroids distance-fn]
  (apply min-key (partial distance-fn p) centroids))

(defn cluster
  "Builds clusters by assigning each point to its
  nearest centroid."
  [data centroids distance-fn]
  (merge
   ;; make sure we don't lose any centroids if they don't
   ;; have any points assigned
   (into {} (map (fn [c] [c nil])
                 centroids))
   (group-by #(nearest-centroid % centroids distance-fn)
             data)))

(defn default-centroid-fn
  "Calculates a centroid for `points`.
  For now, assumes the `points` to be neanderthal vectors."
  [points]
  (lina/scal (/ 1 (count points))
             (reduce (fn [acc curr]
                       (lina/xpy acc curr))
                     (lina/zero (first points))
                     points)))

(defn step
  "Calculates the next generation of centroids from
  the current one."
  [data centroids distance-fn centroid-fn]
  (let [clusters (cluster data centroids distance-fn)]
    (map (fn [c]
           (if-some [ps (seq (get clusters c))]
             (centroid-fn ps)
             c))
         centroids)))

(defn k-means
  "Performs k-means clustering on `data`, given the desired
  number of clusters `k`, a function `distance-fn` that
  computes the distance between two data points, and a
  `max-number-of-iterations` after which to stop the
  algorithm.

  Takes keyword arguments `:k`, `:threshold`,
  `initial-centroids`, and `:centroid-fn`.

  - `:k` is the target number of clusters. If no `:k` is
    passed to [[k-means]], it will assume `:initial-centroids`
    to be present and count those to use for `:k`.
  - After each step, calculate the distance
    between new and previous centroids. Stop the algorithm
    once this is less than `:threshold` in every centroid.
    No given threshold means that `max-number-of-iterations`
    iterations will be done.
  - The `:initial-centroids` designate the starting point
    for the iterations. If none are passed, the algorithm
    will use `:k` random data points.
  - The `:centroid-fn` should take a seq of points (the cluster)
    and return a new point, the corresponding centroid.
    If no `:centroid-fn` is given, `data` is assumed to
    be a seq of neanderthal vectors (for now)."
  [data distance-fn max-number-of-iterations & {:keys [k threshold initial-centroids centroid-fn]}]
  (let [k (or k (count initial-centroids))
        centroid-fn (or centroid-fn default-centroid-fn)
        initial-centroids (or initial-centroids (choose-initial-centroids data k))]
    (loop [centroids initial-centroids
           i 1]
      (let [next-centroids (step data centroids distance-fn centroid-fn)]
        (if (or (>= i max-number-of-iterations)
                (and threshold
                     (every? true?
                             (map (fn [c next-c]
                                    (< (distance-fn c next-c)
                                       threshold))
                                  centroids
                                  next-centroids))))
          (let [result (cluster data next-centroids distance-fn)]
            {:clusters (vals result)
             :iterations i})
          (recur next-centroids
                 (inc i)))))))
