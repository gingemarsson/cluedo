(ns cluedo.util
  (:require [clojure.test :refer [function?]]
            [ysera.test :refer [is is-not is= error?]]
            [cluedo.definitions :refer [suspects weapons rooms]]))

(defn list-contains?
  "Check if list contains key"
  {:test (fn []
           (is= (list-contains? [1 2 3 "test" :test] 2)
                true)
           (is= (list-contains? [1 2 3 4] 7)
                nil))}
  [list key]
  (some (fn [element] (= key element)) list))

(defn flatten-single-elem-sets
  "All single element sets in the collection will be replaced with their element"
  {:test (fn []
           (is= (flatten-single-elem-sets #{1 2 #{3} #{4 5}})
                #{1 2 3 #{4 5}}))}
  [collection]
  (-> (map (fn [elem] (if (and (coll? elem)
                               (= 1 (count elem)))
                        (first elem)
                        elem))
           collection)
      (set)))

(defn remove-from-sets
  "All collections in collection will have the elements in elements-to-remove removed. Non-collections are ignored."
  {:test (fn []
           (is= (remove-from-sets #{1 2 #{3 4} #{4 5 6}} #{4})
                #{1 2 #{3} #{5 6}}))}
  [collection-of-sets elements-to-remove]
  (-> (map (fn [elem] (if (coll? elem)
                        (clojure.set/difference elem elements-to-remove)
                        elem))
           collection-of-sets)
      (set)))

(defn filter-sets
  "All collections in collection will be filtered using the supplied filter function. Non-collections are ignored."
  {:test (fn []
           (is= (filter-sets #{1 2 #{3 4} #{4 5 6}} even?)
                #{1 2 #{4} #{4 6}}))}
  [collection-of-sets filter-function]
  (-> (map (fn [elem] (if (coll? elem)
                        (-> (filter filter-function elem)
                            (set))
                        elem))
           collection-of-sets)
      (set)))