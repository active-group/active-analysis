(ns active-analytics.clustering.k-means
  (:require [active-analytics.linear-algebra :as lina]))

(defn nearest-centroid
  "Finds the centroid closest to a point `p`."
  [p centroids distance-fn]
  (apply min-key (partial distance-fn p) centroids))

(defn cluster
  "Builds clusters by assigning each point to its
  nearest centroid."
  [data centroids distance-fn]
  (merge
   (into {} (map (fn [c] [c nil])
                 centroids))
   (group-by #(nearest-centroid % centroids distance-fn)
             data)))

(defn step
  "Calculates the next generation of centroids from
  the current one."
  [data centroids distance-fn]
  (let [clusters (cluster data centroids distance-fn)]
    (map (fn [c]
           (if-some [ps (seq (get clusters c))]
             (lina/scal (/ 1 (count ps))
                        (reduce (fn [acc curr]
                                  (lina/xpy acc curr))
                                (lina/zero (first ps))
                                ps))
             c))
         centroids)))

(defn k-means
  "Performs k-means clustering on a data set, given the
  desired number of clusters `k`."
  [data k threshold distance-fn initial-centroids]
  (loop [centroids initial-centroids]
    (let [next-centroids (step data centroids distance-fn)]
      (if (every? true?
                  (map (fn [c next-c]
                         (< (distance-fn c next-c)
                            threshold))
                       centroids
                       next-centroids))
        (vals (cluster data next-centroids distance-fn))
        (recur next-centroids)))))
