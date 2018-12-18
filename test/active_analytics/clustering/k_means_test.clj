(ns active-analytics.clustering.k-means-test
  (:require [active-analytics.clustering.k-means :as k-means]
            [clojure.test :refer [deftest is testing]]
            [uncomplicate.neanderthal.core :as neanderthal]
            [uncomplicate.neanderthal.native :as native]))
