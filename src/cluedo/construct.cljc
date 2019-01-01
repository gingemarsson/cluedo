(ns cluedo.construct
  (:require [clojure.test :refer [function?]]
            [ysera.test :refer [is is-not is= error?]]
            [clojure.set]
            [cluedo.util :refer [list-contains?]]
            [cluedo.helper :refer [suspect? weapon? room? card?]]))

(defn create-state
  "Creates an state with the given players."
  ([players] (create-state players []))
  ([players moves]
   {:players players
    :moves   moves}))

(defn create-player
  "Creates an player with given name and amount of cards. If no amout of cards is given 3 is assumed."
  ([name] (create-player name 3))
  ([name num-cards] (create-player name num-cards []))
  ([name num-cards seen-cards]
   {:pre [(every? card? seen-cards)]}
   {:id         name
    :name       name
    :num-cards  num-cards
    :seen-cards seen-cards}))

(defn create-move
  "Creates a move."
  ([asker suspect weapon room answerer]
   (create-move asker suspect weapon room answerer nil))
  ([asker suspect weapon room answerer answer]
   {:pre [(suspect? suspect) (weapon? weapon) (room? room) (or (card? answer) (nil? answer))]}
   {:asker    asker
    :suspect  suspect
    :weapon   weapon
    :room     room
    :answerer answerer
    :answer   answer}))

(defn get-player
  "Get player."
  {:test (fn []
           (is= (->
                 (create-state [(create-player "A")
                                (create-player "B")
                                (create-player "C")
                                (create-player "Me" 5 [:ljusstake :pastor-gron :fru-vit :fru-pafagel :vardagsrummet])]
                               [])
                 (get-player "Me")
                 :num-cards)
                5))}
  [state id]
  (->> (:players state)
       (filter (fn [player] (= (:id player) id)))
       (first)))

(defn num-cards
  "Get number of cards of player with id."
  {:test (fn []
           (is= (->
                 (create-state [(create-player "A" 5 [:froken-rod])
                                (create-player "B")
                                (create-player "C")
                                (create-player "Me" 5 [:ljusstake :pastor-gron :fru-vit :fru-pafagel :vardagsrummet])]
                               [])
                 (num-cards "A"))
                5))}
  [state id]
  (:num-cards (get-player state id)))

(defn seen-cards
  "Get seen cards of player with id."
  {:test (fn []
           (is= (->
                 (create-state [(create-player "A" 5 [:froken-rod])
                                (create-player "B")
                                (create-player "C")
                                (create-player "Me" 5 [:ljusstake :pastor-gron :fru-vit :fru-pafagel :vardagsrummet])]
                               [])
                 (seen-cards "A"))
                #{:froken-rod}))}
  [state id]

  (let [predefined-seen (:seen-cards (get-player state id))
        seen-in-moves   (->> (:moves state)
                             (filter (fn [move] (= (:answerer move) id)))
                             (remove (fn [move] (nil? (:answer move))))
                             (map :answer))]
    (-> (clojure.set/union predefined-seen seen-in-moves)
        (set))))

(defn get-player-ids
  "Get ids of all players"
  [state]
  (map :id (:players state)))

(defn trim-moves
  "Trim moves in state to given number."
  {:test (fn []
           (is= (as->
                 (create-state [(create-player "A" 5 [:froken-rod])
                                (create-player "B")
                                (create-player "C")
                                (create-player "Me" 5 [:ljusstake :pastor-gron :fru-vit :fru-pafagel :vardagsrummet])]
                               [(create-move "A" :fru-vit :ljusstake :hallen "Me")
                                (create-move "B" :fru-vit :rep :matsalen "C")
                                (create-move "Me" :froken-rod :ljusstake :biblioteket "C")
                                (create-move "C" :fru-vit :blyror :vardagsrummet "Me")]) $
                  (trim-moves $ 2)
                  (:moves $)
                  (map :asker $))
                ["A", "B"]))}
  [state num-moves]
  (let [trim-fn (fn [moves length] (subvec moves 0 length))]
    (if (< num-moves (count (:moves state)))
      (update-in state [:moves] trim-fn num-moves)
      state)))

(defn all-players-except
  "Returns the id of all players exept the one given"
  {:test (fn []
           (let [state (create-state [(create-player "A")
                                      (create-player "B")
                                      (create-player "C")
                                      (create-player "Me")]
                                     [])]
             (is= (all-players-except state "C") ["A" "B" "Me"])))}
  [state id]
  (as-> state $
    (:players $)
    (map :id $)
    (remove (fn [elem] (= id elem)) $)))

(defn players-between-asker-and-answerer
  "Gives a list of player id:s between the asker and answerer of a query."
  {:test (fn []
           (let [state (create-state [(create-player "A")
                                      (create-player "B")
                                      (create-player "C")
                                      (create-player "Me")]
                                     [])]
             (is= (players-between-asker-and-answerer state "C" "A") ["Me"])
             (is= (players-between-asker-and-answerer state "A" "C") ["B"])
             (is= (players-between-asker-and-answerer state "Me" "C") ["A" "B"])
             (is= (players-between-asker-and-answerer state "Me" "A") [])
             (is= (players-between-asker-and-answerer state "C" "Me") [])
             (is= (players-between-asker-and-answerer state "A" "B") [])
             (is= (players-between-asker-and-answerer state "B" "A") ["C" "Me"])
             (is= (players-between-asker-and-answerer state "B" "B") [])
             (is= (players-between-asker-and-answerer state "C" nil) ["A" "B" "Me"])
             (is= (players-between-asker-and-answerer state "A" nil) ["B" "C" "Me"])))}
  ([state move]
   (let [asker    (:asker move)
         answerer (:answerer move)]
     (players-between-asker-and-answerer state asker answerer)))
  ([state asker answerer]
   (vec (if (nil? answerer)
          (all-players-except state asker)
          (let [player-ids          (get-player-ids state)
                index-of-asker      (.indexOf player-ids asker)
                index-of-answerer   (.indexOf player-ids answerer)
                greater-index       (max index-of-asker index-of-answerer)
                lower-index         (min index-of-asker index-of-answerer)
                ids-between-indices (cond
                                      (= greater-index lower-index) []
                                      :else (subvec (vec player-ids) (inc lower-index) greater-index))]
            (cond
              (< index-of-asker index-of-answerer) ids-between-indices
              (> index-of-asker index-of-answerer) (remove (fn [id] (or (list-contains? ids-between-indices id)
                                                                        (= id asker)
                                                                        (= id answerer))) player-ids)))))))