(defproject asols "0.1.0-SNAPSHOT"
  :description "Algorithm of Structure Optimization on Learning Stage"
  :url "https://github.com/dferens/asols"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2850"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.omcljs/om "0.8.8"]
                 [sablono "0.3.4"]
                 [ring/ring-core "1.3.2"]
                 [ring/ring-devel "1.3.2"]
                 [dorothy "0.0.6"]
                 [jarohen/chord "0.6.0"]
                 [compojure "1.3.1"]]

  :plugins [[lein-cljsbuild "1.0.4"]]

  :main ^:skip-aot asols.core
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]]}
             :uberjar {:aot :all}}

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src"]
              :compiler {:output-to "resources/public/js/compiled/main.js"
                         :output-dir "resources/public/js/compiled/out"
                         :source-map true
                         :optimizations :none}}]})
