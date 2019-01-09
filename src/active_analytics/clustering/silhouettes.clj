(ns active-analytics.clustering.silhouettes
  (:require [active-analytics.util :as util]))

(defn average-distance
  "Calculates the average distance from a point
  `x` to a set of points `xs` via `distance-fn`.
  `x` itself is disregarded if contained in `xs`."
  [x xs distance-fn]
  (->> xs
       (remove (partial = x))
       (map (partial distance-fn x))
       util/average-or-zero))

(defn average-distance-to-closest-cluster
  "Calculates the minimal average distance between
  a point and a collection of clusters."
  [x clusters distance-fn]
  (->> clusters
       (map #(average-distance x % distance-fn))
       (apply min)))

(defn silhouette-width
  "Calculates the silhouette width of a point `x`,
  where `x` lies in `containing-cluster`, and
  `other-clusters` does not contain `containing-cluster`.
  This measures how well `x` fits into its cluster.
  A positive value close to 1 means a good fit,
  0 means it is unclear where `x` belongs,
  and a negative value suggests that `x` should
  probably be inside the neighbouring cluster."
  [x containing-cluster other-clusters distance-fn]
  (let [avg-distance-within-cluster (average-distance x containing-cluster distance-fn)
        neighbour-distance (average-distance-to-closest-cluster x other-clusters distance-fn)]
    (/ (- neighbour-distance
          avg-distance-within-cluster)
       (max avg-distance-within-cluster
            neighbour-distance))))

(defn average-silhouette-width
  "Calculates the average of all silhouette widths
  of points in a `cluster`."
  [cluster other-clusters distance-fn]
  (->> cluster
       (map #(silhouette-width % cluster other-clusters distance-fn))
       util/average-or-zero))

(defn total-average-silhouette-width
  "Calculates the average of all average silhouette
  widths over a collection of `clusters`.
  Analogous to the silhouette width, this provides a
  measurement of how good the overall clustering is."
  [clusters distance-fn]
  (->> clusters
       (map #(let [other-clusters (remove (partial = %) clusters)]
               (average-silhouette-width % other-clusters distance-fn)))
       util/average-or-zero))
