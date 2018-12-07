(ns active-analytics.clustering.k-means
  (:require [active-analytics.linear-algebra :as lina]))

(defn nearest-centroid
  [p centroids distance-fn]
  (apply min-key (partial distance-fn p) centroids))

(defn cluster
  [data centroids distance-fn]
  (merge
   (into {} (map (fn [c] [c nil])
                 centroids))
   (group-by #(nearest-centroid % centroids distance-fn)
             data)))

(defn iterate
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
  [data k threshold distance-fn initial-cendroids]
  (loop [centroids initial-cendroids]
    (let [next-centroids (iterate data centroids distance-fn)]
      (if (every? true?
                  (map (fn [c next-c]
                         (< (distance-fn c next-c)
                            threshold))
                       centroids
                       next-centroids))
        (cluster data next-centroids distance-fn)
        (recur next-centroids)))))
