(defproject asols "0.1.0-SNAPSHOT"
  :description "Algorithm of Structure Optimization on Learning Stage"
  :url "https://github.com/dferens/asols"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0-RC1"]
                 [org.clojure/clojurescript "0.0-3269"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/data.csv "0.1.2"]
                 [org.omcljs/om "0.8.8"]
                 [com.climate/claypoole "1.0.0"]
                 [com.taoensso/timbre "3.4.0"]
                 [net.mikera/vectorz-clj "0.29.0"]
                 [net.mikera/core.matrix.stats "0.5.0"]
                 [compojure "1.3.4"]
                 [dorothy "0.0.6"]
                 [figwheel "0.2.6"]
                 [incanter "1.5.6"]
                 [jarohen/chord "0.6.0"]
                 [prismatic/om-tools "0.3.11"]
                 [ring/ring-core "1.3.2"]
                 [ring/ring-devel "1.3.2"]
                 [ring/ring-jetty-adapter "1.4.0-beta2"]
                 [sablono "0.3.4"]]

  :bower-dependencies [[flat-ui "2.2.2"]
                       [highcharts "v4.1.5"]]
  :bower {:directory "resources/public/vendor"}

  :main ^:skip-aot asols.core
  :target-path "target/%s"
  :source-paths ["target/generated/src/clj" "src/clj"]
  :resource-paths ["resources/"]
  :prep-tasks [["cljx" "once"] "javac" "compile"]

  :profiles {:dev {:plugins [[lein-cljsbuild "1.0.4"]
                             [lein-haml-sass "0.2.7-SNAPSHOT"]
                             [lein-bower "0.5.1"]
                             [lein-figwheel "0.2.6"]
                             [lein-pdo "0.1.1"]
                             [com.keminglabs/cljx "0.6.0"]
                             [com.jakemccrary/lein-test-refresh "0.8.0"]]
                   :dependencies [[pjstadig/humane-test-output "0.7.0"]]
                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]}
             :uberjar {:aot :all}}

  :aliases {"dev" ["pdo" ["cljx" "auto"]
                         ["sass" "auto"]
                         ["figwheel"]]
            "release" ["do"
                       ["cljx" "once"]
                       ["sass" "once"]
                       ["cljsbuild" "once"]
                       "uberjar"]}

  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/generated/src/clj"
                   :rules :clj}

                  {:source-paths ["src/cljx"]
                   :output-path "target/generated/src/cljs"
                   :rules :cljs}]}

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src/cljs" "target/generated/src/cljs"]
              :compiler {:output-to "resources/public/js/compiled/main.js"
                         :output-dir "resources/public/js/compiled/out"
                         :source-map true
                         :optimizations :none}}]}

  :sass {:src "resources/public/sass"
         :output-directory "resources/public/css"
         :output-extension "css"}

  :figwheel {:css-dirs ["resources/public/css"]})
