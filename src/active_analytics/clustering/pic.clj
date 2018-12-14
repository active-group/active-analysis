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

(defn initial-vector
  "Creates an initial vector for PIC clustering.
  So far, using a range (thus treating each point as
  if it were in its own cluster) over the proposed
  weighted initial vector has proven more fruitful."
  [affinity-matrix & {:keys [range?] :or {range? true}}]
  (if range?
    (lina/vctr (range (lina/mrows affinity-matrix)))
    (let [volume (lina/sum affinity-matrix)
          alpha (/ 1 volume)]
      (->> affinity-matrix
           lina/rows
           (map lina/sum)
           lina/vctr
           (lina/scal alpha)))))

(defn scale
  "Calculates the 'scale' of a vector `v`.
  This is the difference between the greatest and the
  smallest vector entry."
  [v]
  (let [max (lina/maximum v)
        min (lina/minimum v)]
    (- max min)))

(defn threshold
  "Calculates a threshold which can be used to determine
  when to stop the power iteration, that is, when local
  convergence has been reached."
  [number-of-data-points]
  (max
   (/ 1e-5
      number-of-data-points)
   1e-8))

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
  [affinity-matrix max-number-of-iterations]
  (let [v-0 (initial-vector affinity-matrix)
        dimension (lina/mrows affinity-matrix)
        normalized-affinity-matrix (normalize-affinity-matrix affinity-matrix)
        epsilon (threshold dimension)]
    (loop [i 1
           v v-0
           prev-delta (lina/vctr (repeat dimension 0))]
      (let [v-new (step normalized-affinity-matrix v)
            delta (lina/vect-abs (lina/subtract v-new v))]
        (if (or (>= i
                    max-number-of-iterations)
                (<= (lina/nrmi (lina/vect-abs (lina/subtract delta
                                                             prev-delta)))
                    epsilon))
          v-new
          (recur (inc i)
                 v-new
                 delta))))))

(defn pic
  "Performs power iteration clustering with an `affinity-matrix`,
  the desired number of clusters `k`, and a number of iterations
  and threshold which determine when to terminate the algorithm."
  [affinity-matrix k max-number-of-iterations threshold]
  (let [v-final (pic-only affinity-matrix max-number-of-iterations threshold)
        min (lina/minimum v-final)
        max (lina/maximum v-final)
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
