(ns active-analytics.clustering.pic
  (:require [active-analytics.clustering.k-means :as k-means]
            [active-analytics.linear-algebra :as lina]))

(defn create-affinity-matrix!
  "Creates an affinity matrix from `data` and a function
  that calculates the distance between two data points."
  [data distance-fn]
  (let [n (count data)
        m (lina/ge n n)
        _ (dorun (map-indexed (fn [i x]
                                (dorun (map (fn [j]
                                              (let [y (nth data j)
                                                    dist (distance-fn x y)]
                                                (lina/entry! m i j dist)
                                                (lina/entry! m j i dist)))
                                            (range i n))))
                              data))]
    m))

(defn degree-matrix
  "Calculates the degree matrix of a matrix `m`.
  This is a diagonal matrix where the entry of each row
  is the row sum of the corresponding row of `m`."
  [m]
  (lina/gd (lina/mrows m)
           (map lina/sum
                (lina/rows m))))

(defn normalize-affinity-matrix
  "Normalizes an affinity matrix by dividing each entry
  by its row sum."
  [m]
  (-> m
      degree-matrix
      lina/invert
      (lina/mm m)))

(defn create-normalized-affinity-matrix!
  "Creates a normalized affinity matrix from `data` and a
  function that calculates the distance between two data points."
  [data distance-fn]
  (normalize-affinity-matrix (create-affinity-matrix! data
                                                      distance-fn)))

(defn step
  "Given a (normalized) affinity matrix and a vector `v`,
  calculate the next vector by using power iteration."
  [affinity-matrix v]
  (let [v-new (lina/mv affinity-matrix v)
        norm (lina/nrm1 v-new)]
    (lina/scal (/ 1 norm)
               v-new)))

(defn pic-only
  "Performs 'pure' power iteration clustering, that is,
  without using another clustering algorithm on the result.
  Hence this returns a real-valued vector with the same
  dimension as `affinity-matrix`."
  [affinity-matrix v-0 max-number-of-iterations threshold]
  (loop [i 1
         v v-0]
    (let [v-new (step affinity-matrix v)]
      (if (or (>= i
                  max-number-of-iterations)
              (<= (lina/nrm1 (lina/subtract v v-new))
                  threshold))
        v-new
        (recur (inc i)
               v-new)))))

(defn pic
  "Performs power iteration clustering with an `affinity-matrix`,
  the desired number of clusters `k`, and a number of iterations
  and threshold which determine when to terminate the algorithm."
  [affinity-matrix k v-0 max-number-of-iterations threshold]
  (let [v-final (pic-only affinity-matrix v-0 max-number-of-iterations threshold)
        min (lina/entry v-final (lina/imin v-final))
        max (lina/entry v-final (lina/imax v-final))
        initial-centroids (map #(+ min
                                   (* (/ (- max min)
                                         (dec k))
                                      %))
                               (range k))
        glue-with-index #(map-indexed (fn [i x]
                                        (lina/vctr [x i]))
                                      %)
        k-means-clusters (k-means/k-means (glue-with-index v-final)
                                          k
                                          threshold
                                          ;; FIXME: is using neanderthal to circumvent boxing possible?
                                          (fn [v w]
                                            (lina/abs (- (first v)
                                                         (first w))))
                                          (glue-with-index initial-centroids))]
    (map (fn [cluster]
           (map (comp int second)
                cluster))
         k-means-clusters)))
