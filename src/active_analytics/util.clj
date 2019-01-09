(ns active-analytics.util)

(defn average-or-zero
  "Calculates the average of a seq of numbers.
  For an empty seq, 0 is returned."
  [xs]
  (if (seq xs)
    (/ (apply + xs)
       (count xs))
    0))
