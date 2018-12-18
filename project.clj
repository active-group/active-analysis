(defproject de.active-group/active-analytics "0.1.0"
  :description "Active Analytics: Clojure data analysis algorithms in use at Active Group"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/test.check "0.10.0-alpha3"]
                 ;;[org.nd4j/nd4j-native-platform "0.9.1"]
                 [uncomplicate/neanderthal "0.20.4"]]
  :exclusions [[org.jcuda/jcuda-natives :classifier "apple-x86_64"]
               [org.jcuda/jcublas-natives :classifier "apple-x86_64"]])
