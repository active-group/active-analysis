(defproject de.active-group/active-analytics "0.3.0"
  :description "Active Analytics: Clojure data analysis algorithms in use at Active Group"
  :url "https://github.com/active-group/active-analytics"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/test.check "0.10.0-alpha3"]
                 ;;[org.nd4j/nd4j-native-platform "0.9.1"]
                 [uncomplicate/neanderthal "0.20.4"]
                 [org.clojure/math.combinatorics "0.1.4"]]
  :exclusions [[org.jcuda/jcuda-natives]
               [org.jcuda/jcublas-natives]])
