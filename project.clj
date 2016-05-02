(defproject logical-programming "0.1.2-SNAPSHOT"
  :description "Prolog implementation & library for Clojure"
  :url "https://github.com/m1trix/scm-prolog"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot logic.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
