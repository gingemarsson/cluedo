(defproject cluedo "0.1.0-SNAPSHOT"
  :description "A functional cluedo solver"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [ysera "1.2.0"]
                 [org.clojure/math.combinatorics "0.1.4"]]
  :main ^:skip-aot cluedo.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
