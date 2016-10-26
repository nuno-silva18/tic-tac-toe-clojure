(defproject tic-tac-toe "0.1.0-SNAPSHOT"
  :description "Tic-Tac-Toe written in Clojure"
  :url "..."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot tic-tac-toe.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
