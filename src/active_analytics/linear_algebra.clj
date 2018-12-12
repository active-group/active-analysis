(ns active-analytics.linear-algebra
  (:require [uncomplicate.neanderthal.core :as neanderthal]
            [uncomplicate.neanderthal.linalg :as linalg]
            [uncomplicate.neanderthal.math :as math]
            [uncomplicate.neanderthal.native :as native]))

;; TODO:
;; - functions to create vectors/matrices/numbers
;;   -> see/use neanderthal.core/vctr, /ge, ...
;; - use the 'real' namespace for more efficient functions where applicable

(defn ge
  ([m n]
   (neanderthal/ge native/native-double
                   m
                   n))
  ([a]
   (neanderthal/ge native/native-double
                   a)))

(defn gd
  [n source]
  (neanderthal/gd native/native-double
                  n
                  source))

(def mrows neanderthal/mrows)

(def rows neanderthal/rows)

(def sum neanderthal/sum)

(defn mm
  [a b]
  (neanderthal/mm a b))

(defn trf
  [a]
  (linalg/trf a))

(defn tri
  [a]
  (linalg/tri a))

(def invert (comp tri trf))

(defn symmetric?
  "FIXME: this is not identical to neanderthal/symmetric; use sy for creation"
  [a]
  (= (neanderthal/trans a)
     a))

(defmacro scal
  "Multiply a scalar `a` with a matrix `m`."
  [a m]
  `(neanderthal/scal ~a ~m))

(defn nrm1
  "Calculates the 1-norm of a matrix `m`."
  [m]
  (neanderthal/nrm1 m))

(defmacro mv
  "Multiplies a matrix `m` with a vector `v`."
  [m v]
  `(neanderthal/mv ~m
                   ~v))

(defn xpy
  "Adds matrices or vectors."
  [m1 m2]
  (neanderthal/xpy m1
                   m2))

(defn subtract
  "Subtracts the matrix `m2` from the matrix `m1`."
  [m1 m2]
  (neanderthal/axpy -1
                    m1
                    m2))

(defmacro dim
  [v]
  `(neanderthal/dim ~v))

(defmacro zero
  [m]
  `(neanderthal/zero ~m))

(defn vctr
  [source]
  (neanderthal/vctr native/native-double source))

(defn entry
  [x i]
  (neanderthal/entry x i))

(defn entry!
  ([x i val]
   (neanderthal/entry! x i val))
  ([x i j val]
   (neanderthal/entry! x i j val)))

(defn imin
  [x]
  (neanderthal/imin x))

(defn imax
  [x]
  (neanderthal/imax x))

(defn abs
  [x]
  (math/abs x))
