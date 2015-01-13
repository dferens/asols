(defproject asols "0.1.0-SNAPSHOT"
  :description "Algorithm of Structure Optimization on Learning Stage"
  :url "https://github.com/dferens/asols"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot asols.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
