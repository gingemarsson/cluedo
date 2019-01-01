(ns cluedo.all
  (:require [ysera.test :refer [deftest is]]
            [clojure.test :refer [successful? run-tests]]
            [cluedo.core]
            [cluedo.construct]
            [cluedo.util]
            [cluedo.helper]))

(deftest test-all
  "Bootstrapping with the required namespaces, finds all the clojure.* namespaces (except this one),
         requires them, and runs all their tests."
  (let [namespaces (->> (all-ns)
                        (map str)
                        (filter (fn [x] (re-matches #"cluedo\..*" x)))
                        (remove (fn [x] (= "cluedo.all" x)))
                        (map symbol))]
    (is (successful? (time (apply run-tests namespaces))))))
