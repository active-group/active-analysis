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
  [x xs dissimilarity-fn]
  (- (apply +
          (map (partial dissimilarity-fn x)
               xs))
     (dissimilarity-fn x x)))

(defn choose-initial-medoids-randomly
  "Randomly chooses a set of medoids to start out with."
  [xs k]
  (->> xs
       shuffle
       (take k)
       (into #{})))

(defn first-pam-medoid
  "Determines the first medoid for the PAM BUILD process."
  [xs dissimilarity-fn]
  (apply min-key
         #(dissimilarity-sum % xs dissimilarity-fn)
         xs))

(defn min-medoid-distance
  "Calculates the distance to the closest medoid."
  [x medoids dissimilarity-fn]
  (->> medoids
       (map (partial dissimilarity-fn x))
       (apply min)))

(defn contribution
  "Calculates the contribution of `x` to the change in gain
  when adding `candidate` as additional medoid."
  [candidate x medoids dissimilarity-fn]
  (max 0
       (- (min-medoid-distance x medoids dissimilarity-fn)
          (dissimilarity-fn candidate x))))

(defn gain
  "Calculates the gain associated with the choice
  of a medoid candidate."
  [candidate xs medoids dissimilarity-fn]
  (->> xs
       (remove (partial = candidate))
       (map #(contribution candidate % medoids dissimilarity-fn))
       (apply +)))

(defn next-pam-medoid
  "Determines the best additional medoid, that is,
  the point with maximal gain w.r.t. the currently
  chosen medoids."
  [xs medoids dissimilarity-fn]
  (apply max-key
         #(gain % xs medoids dissimilarity-fn)
         xs))

(defn pam-build
  "Performs the PAM BUILD initialization. That is,
  this greedily finds an initial set of medoids by
  iteratively choosing points which minimize dissimilarity."
  [xs dissimilarity-fn k]
  (let [first-medoid (first-pam-medoid xs dissimilarity-fn)]
    (loop [medoids #{first-medoid}
           residual-data (remove (partial = first-medoid) xs)]
      (if (= (count medoids)
             k)
        medoids
        (let [next-medoid (next-pam-medoid residual-data medoids dissimilarity-fn)]
          (recur (conj medoids next-medoid)
                 (remove (partial = next-medoid) residual-data)))))))

(defn nearest-medoid
  "Finds the medoid that is closest to a point `x`."
  [x medoids dissimilarity-fn]
  (apply min-key
         (partial dissimilarity-fn x)
         medoids))

(defn cluster
  "Groups the data points `xs` by assigning each to its
  nearest medoid."
  [xs medoids dissimilarity-fn]
  (group-by #(nearest-medoid % medoids dissimilarity-fn)
            xs))

(defn find-medoid
  "Finds the medoid of a set of data points `xs`, that is,
  the point with the lowest sum of dissimilarities."
  [xs dissimilarity-fn]
  (apply min-key
         #(dissimilarity-sum % xs dissimilarity-fn)
         xs))

(defn step
  "Calculates the next generation of medoids from the current one."
  [xs medoids dissimilarity-fn]
  (let [clusters (cluster xs medoids dissimilarity-fn)]
    (into #{}
          (map #(find-medoid % dissimilarity-fn)
               (vals clusters)))))

(defn dissimilarity-function
  "Returns a function to calculate the dissimilarity
  between two data points. If `cache?` is `true`, a map
  of dissimilarities is built and the resulting function
  will be a closure that does lookups inside the map.
  Otherwise `distance-fn` is returned."
  [xs distance-fn cache?]
  (if cache?
    (let [dissimilarities (dissimilarity-map xs distance-fn)]
      (fn [x y]
        (dissimilarity x y dissimilarities)))
    distance-fn))

(defn k-medoids
  "Performs k-medoid clustering on the data points `xs`,
  given the desired number of clusters `k` and a function
  `distance-fn` that computes the distance between two
  points.

  Accepts the keyword arguments `:medoid-init-mode` and
  `:cache?`.

  - `:medoid-init-mode` determines the way the initial set
    of medoids is created. The default value, `:random`,
    corresponds to random choice of initial medoids, while
    `:pam` utilizes the PAM BUILD algorithm to greedily
    choose `k` 'good' starting medoids.
  - If `:cache?` is `true` (the default), a map of dissimilarities
    is built which is then used to perform lookups instead
    of distance recalculations."
  [xs distance-fn k
   & {:keys [medoid-init-mode cache?]
      :or {medoid-init-mode :random cache? true}}]
  {:pre [(contains? #{:random :pam} medoid-init-mode)
         (boolean? cache?)]}
  (let [dissimilarity-fn (dissimilarity-function xs distance-fn cache?)
        initial-medoids (case medoid-init-mode
                          :random (choose-initial-medoids-randomly xs k)
                          :pam (pam-build xs dissimilarity-fn k))]
    (loop [medoids initial-medoids
           i 1]
      (let [next-medoids (step xs medoids dissimilarity-fn)]
        (if (= medoids
               next-medoids)
          {:clusters (vals (cluster xs medoids dissimilarity-fn))
           :iterations i}
          (recur next-medoids (inc i)))))))
