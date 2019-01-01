(ns cluedo.helper
  (:require [clojure.test :refer [function?]]
            [ysera.test :refer [is is-not is= error?]]
            [clojure.set]
            [cluedo.definitions :refer [suspects weapons rooms]]
            [cluedo.util :refer [list-contains?]]))

(defn suspect?
  "Check if key is an suspect"
  {:test (fn []
           (is= (suspect? :pastor-gron)
                true)
           (is= (suspect? :vintertradgarden)
                nil))}
  [key]
  (list-contains? suspects key))

(defn weapon?
  "Check if key is an weapon"
  {:test (fn []
           (is= (weapon? :dolk)
                true)
           (is= (weapon? :pastor-gron)
                nil))}
  [key]
  (list-contains? weapons key))

(defn room?
  "Check if key is an room"
  {:test (fn []
           (is= (room? :vintertradgarden)
                true)
           (is= (room? :dolken)
                nil))}
  [key]
  (list-contains? rooms key))

(defn card?
  "Check if key is any card"
  {:test (fn []
           (is= (card? :vintertradgarden)
                true)
           (is= (card? ::nope)
                nil))}
  [key]
  (or (suspect? key) (weapon? key) (room? key)))

(defn all-cards-of-type
  "Get all cards of a type"
    {:test (fn []
             (is= (all-cards-of-type :room)
                  #{:koket
                    :danssalongen
                    :vintertradgarden
                    :matsalen
                    :biljardrummet
                    :biblioteket
                    :hallen
                    :vardagsrummet
                    :arbetsrummet}))}
  [type]
  (case type
    :suspect suspects
    :weapon weapons
    :room rooms))

(defn type-checker-for-type
  "Get type checker for specified type"
      {:test (fn []
               (is= ((type-checker-for-type :suspect) :koket) nil)
               (is= ((type-checker-for-type :weapon) :dolk) true)
               (is= ((type-checker-for-type :room) :koket) true))}
  [type]
  (case type
    :suspect suspect?
    :weapon weapon?
    :room room?
    ))

(defn type-of
  "Get type of card"
  {:test (fn []
           (is= (type-of :koket) :room)
           (is= (type-of :dolk) :weapon)
           (is= (type-of :froken-rod) :suspect))}
  [card]
  (cond 
    (suspect? card) :suspect
    (weapon? card) :weapon
    (room? card) :room))

(defn envelope-set-to-sorted-seq
  [set]
  (sort-by (fn [x] (cond
                     (suspect? x) 0
                     (weapon? x) 1
                     (room? x) 2)) set))