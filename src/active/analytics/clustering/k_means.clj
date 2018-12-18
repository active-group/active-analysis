(ns active-analytics.clustering.k-means
  (:require [active-analytics.linear-algebra :as lina]))

(defn initial-centroids
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
  "Performs k-means clustering on a data set, given the
  desired number of clusters `k`.
  If no `centroid-fn` is given, assumes the data to be
  neanderthal vectors (for now)."
  ([data distance-fn k threshold max-number-of-iterations]
   (k-means data distance-fn k threshold max-number-of-iterations (initial-centroids data k)))
  ([data distance-fn k threshold max-number-of-iterations initial-centroids]
   (k-means data distance-fn default-centroid-fn k threshold max-number-of-iterations initial-centroids))
  ([data distance-fn centroid-fn k threshold max-number-of-iterations initial-centroids]
   (loop [centroids initial-centroids
          i 1]
     (let [next-centroids (step data centroids distance-fn centroid-fn)]
       (if (or (>= i max-number-of-iterations)
               (every? true?
                       (map (fn [c next-c]
                              (< (distance-fn c next-c)
                                 threshold))
                            centroids
                            next-centroids)))
         (vals (cluster data next-centroids distance-fn))
         (recur next-centroids
                (inc i)))))))
