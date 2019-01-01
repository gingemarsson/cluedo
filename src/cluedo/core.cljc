(ns cluedo.core
  (:require [ysera.test :refer [is is-not is= error?]]
            [clojure.set]
            [clojure.math.combinatorics]
            [cluedo.predefined-games :refer [game-one game-two]]
            [cluedo.construct :refer [create-state
                                      create-player
                                      create-move
                                      players-between-asker-and-answerer
                                      get-player-ids
                                      seen-cards
                                      num-cards
                                      trim-moves
                                      all-players-except]]
            [cluedo.util :refer [list-contains?
                                 flatten-single-elem-sets
                                 remove-from-sets
                                 filter-sets]]
            [cluedo.helper :refer [suspect?
                                   weapon?
                                   room?
                                   card?
                                   all-cards-of-type
                                   type-checker-for-type
                                   type-of
                                   envelope-set-to-sorted-seq]]
            [cluedo.definitions :refer [suspects weapons rooms]])
  (:gen-class))

(defn inferred-have
  "The cards we can infer a player have based on which queries they have answered."
  {:test (fn []
           (let [state (create-state [(create-player "A")
                                      (create-player "B")
                                      (create-player "C")
                                      (create-player "Me")]
                                     [(create-move "Me" :froken-rod :ljusstake :biblioteket "A" :froken-rod)])]
             (is= (inferred-have state "A")
                  #{#{:froken-rod :ljusstake :biblioteket}})))}
  [state id]
  (->> state
       (:moves)
       (filter (fn [move] (= id (:answerer move))))
       (map (fn [move] #{(:suspect move) (:weapon move) (:room move)}))
       (set)))

(defn inferred-have-not
  "The cards we can infer a player do not have based on what they have not answered."
  {:test (fn []
           (let [state (create-state [(create-player "A")
                                      (create-player "B")
                                      (create-player "C")
                                      (create-player "Me")]
                                     [(create-move "Me" :pastor-gron :ljusstake :biblioteket "B")])]
             (is= (inferred-have-not state "A")
                  #{:pastor-gron :ljusstake :biblioteket})))}
  [state id]
  (->> state
       (:moves)
       (filter (fn [move] (list-contains? (players-between-asker-and-answerer state move) id)))
       (map (fn [move] [(:suspect move) (:weapon move) (:room move)]))
       (flatten)
       (set)))
(def m-inferred-have-not (memoize inferred-have-not))

(defn inferred-nobody-has
  "The cards all players have skipped showing."
  {:test (fn []
           (let [state (create-state [(create-player "A")
                                      (create-player "B")
                                      (create-player "C")
                                      (create-player "Me")]
                                     [(create-move "Me" :pastor-gron :ljusstake :biblioteket "B")
                                      (create-move "A" :pastor-gron :dolk :matsalen nil)])]
             (is= (inferred-nobody-has state)
                  #{:pastor-gron})))}
  [state]
  (let [player-ids                (get-player-ids state)
        function                  (fn [id] (m-inferred-have-not state id))
        sets-of-inferred-have-not (->> (map function player-ids)
                                       (map set))]
    (apply clojure.set/intersection sets-of-inferred-have-not)))

(defn known-have
  "The cards we have seen and inferred a player has."
  {:test (fn []
           (let [state (create-state [(create-player "A")
                                      (create-player "B")
                                      (create-player "C")
                                      (create-player "Me")]
                                     [(create-move "C" :pastor-gron :dolk :matsalen "A")
                                      (create-move "Me" :froken-rod :ljusstake :biblioteket "A" :froken-rod)])]
             (is= (known-have state "A")
                  #{:froken-rod #{:pastor-gron :dolk :matsalen}})))}
  [state id]
  (as->
   (inferred-have state id) $
    (remove-from-sets $ (m-inferred-have-not state id))
    (flatten-single-elem-sets $)
    (clojure.set/union $ (seen-cards state id))
    (remove (fn [set] (and (set? set)
                           (some (fn [elem] (list-contains? $ elem)) set))) $)
    (set $)))

(defn known-others-have
  "The cards we have seen and inferred players other than id has."
  {:test (fn []
           (let [state (create-state [(create-player "A")
                                      (create-player "B")
                                      (create-player "C")
                                      (create-player "Me")]
                                     [(create-move "C" :pastor-gron :dolk :matsalen "A")
                                      (create-move "Me" :froken-rod :ljusstake :biblioteket "A" :froken-rod)])]
             (is= (known-others-have state "Me")
                  #{:froken-rod})))}
  [state id]
  (->> (apply clojure.set/union (->> (all-players-except state id)
                                     (map (fn [other-player-id] (known-have state other-player-id)))))
       (remove set?)
       (set)))

(defn possible-cards
  "Returnes the set of cards not known to be owned by any player."
  {:test (fn []
           (let [state (create-state [(create-player "A")
                                      (create-player "B")
                                      (create-player "C")
                                      (create-player "Me")]
                                     [(create-move "C" :pastor-gron :dolk :matsalen "A")
                                      (create-move "Me" :froken-rod :ljusstake :biblioteket "A" :froken-rod)
                                      (create-move "Me" :overste-senap :dolk :biblioteket "B" :dolk)
                                      (create-move "Me" :overste-senap :ljusstake :matsalen "C" :matsalen)])]
             (is= (possible-cards state :suspect)
                  #{:fru-pafagel :professor-plommon :overste-senap :fru-vit})))}
  [state card-type]
  (as-> (apply clojure.set/union (->> (get-player-ids state)
                                      (map (fn [other-player-id] (known-have state other-player-id))))) $
    (filter (fn [elem] ((type-checker-for-type card-type) elem)) $)
    (clojure.set/difference (all-cards-of-type card-type) $)))

(defn only-one-left-of-type?
  "Returnes true of the specified card is the only card of that type which we do not know the owner of."
  {:test (fn []
           (let [state (create-state [(create-player "A")
                                      (create-player "B")
                                      (create-player "C")
                                      (create-player "Me")]
                                     [(create-move "Me" :pastor-gron :dolk :biblioteket "A" :pastor-gron)
                                      (create-move "Me" :froken-rod :dolk :biblioteket "A" :froken-rod)
                                      (create-move "Me" :fru-pafagel :dolk :biblioteket "B" :fru-pafagel)
                                      (create-move "Me" :fru-vit :dolk :biblioteket "B" :fru-vit)
                                      (create-move "Me" :overste-senap :dolk :biblioteket "C" :overste-senap)])]
             (is= (only-one-left-of-type? state :professor-plommon)
                  true)))}
  [state card]
  (let [type         (type-of card)
        left-of-type (possible-cards state type)]
    (and (= (count left-of-type) 1)
         (= card (first left-of-type)))))

(defn refined-known-have
  "The cards we have seen and inferred a player has, minus the ones we know another player has."
  {:test (fn []
           (let [state (create-state [(create-player "A")
                                      (create-player "B")
                                      (create-player "C")
                                      (create-player "Me")]
                                     [(create-move "C" :pastor-gron :dolk :matsalen "A")
                                      (create-move "Me" :froken-rod :ljusstake :biblioteket "A" :froken-rod)
                                      (create-move "Me" :overste-senap :dolk :biblioteket "B" :dolk)
                                      (create-move "Me" :overste-senap :ljusstake :matsalen "C" :matsalen)])]
             (is= (refined-known-have state "A")
                  #{:froken-rod :pastor-gron})))}
  [state id]
  (let [known-have        (known-have state id)
        known-others-have (known-others-have state id)
        filter-function   (fn [x] (not (only-one-left-of-type? state x)))]
    (-> (remove-from-sets known-have known-others-have)
        (filter-sets filter-function)
        (flatten-single-elem-sets))))
(def m-refined-known-have (memoize refined-known-have))

(defn whole-hand-known?
  "Returns true if we know all cards of the specified player."
  [state id]
  (let [total-num-cards (num-cards state id)
        known-num-cards (count (->> (m-refined-known-have state id)
                                    (remove set?)))]
    (= total-num-cards known-num-cards)))

(defn all-cards-not-in-hand
  "All cards not in in the hand of specified player. Hand is calculated using refined-known-have."
  [state id]
  (as-> (clojure.set/union
         (all-cards-of-type :suspect)
         (all-cards-of-type :weapon)
         (all-cards-of-type :room)) $
    (clojure.set/difference $ (m-refined-known-have state id))))

(defn refined-inferred-nobody-has
  "The cards all players have skipped showing or are known to not have due to their hand beeing full."
  [state]
  (let [player-ids                (get-player-ids state)
        function                  (fn [id] (if (whole-hand-known? state id)
                                             (all-cards-not-in-hand state id)
                                             (m-inferred-have-not state id)))
        sets-of-inferred-have-not (->> (map function player-ids)
                                       (map set))]
    (apply clojure.set/intersection sets-of-inferred-have-not)))
(def m-refined-inferred-nobody-has (memoize refined-inferred-nobody-has))

(defn refined-inferred-unknown-owner
  "Infer which card we do not know the owner of based on refined-know-have and refined-inferred-nobody-has."
  [state]
  (let [all-cards        (clojure.set/union
                          (all-cards-of-type :suspect)
                          (all-cards-of-type :weapon)
                          (all-cards-of-type :room))
        known-held-cards (->> (apply clojure.set/union
                                     (->> (get-player-ids state)
                                          (map (fn [id] (m-refined-known-have state id)))))
                              (remove set?))
        nobody-has       (m-refined-inferred-nobody-has state)]
    (clojure.set/difference all-cards known-held-cards nobody-has)))

(defn free-owners-to-fill
  "Returns a list of owners which have free spots. If anybody has multiple spots they are counted multiple times. Uses refined-known-have."
  [state]
  (let [envelope-counts {:id  :envelope
                         :num (- 3 (count (m-refined-inferred-nobody-has state)))}
        player-ids      (get-player-ids state)
        player-counts   (map (fn [id] (let [total-num-cards (num-cards state id)
                                            known-num-cards (count (->> (m-refined-known-have state id)
                                                                        (remove set?)))]
                                        {:id  id
                                         :num (- total-num-cards known-num-cards)})) player-ids)]
    (->> (conj player-counts envelope-counts)
         (map (fn [{id  :id
                    num :num}] (repeat num id)))
         (flatten))))

(defn valid-assignment?
  "Check that a player does not NOT have the card using refined-inferred-nobody-has. For the envelope, check that there is no other card of that type."
  [state {card :card
          id   :owner}]
  (cond
    (= id :envelope) (-> (type-checker-for-type (type-of card))
                         (filter (m-refined-inferred-nobody-has state))
                         (empty?))
    :else (and
           (not (list-contains? (m-inferred-have-not state id) card)))))
(def m-valid-assignment? (memoize valid-assignment?))

(defn valid-assignments-to-player?
  "Check that a player does not have to have something else. Uses refined-known-have."
  [state id cards]
  (cond
    (= id :envelope) (and
                      (> 2 (count (filter suspect? cards)))
                      (> 2 (count (filter weapon? cards)))
                      (> 2 (count (filter room? cards))))
    :else (every? (fn [set] (some (fn [elem] (list-contains? cards elem)) set))
                  (filter set? (m-refined-known-have state id)))))

(defn valid-assignments?
  "Checks if a given set of assignments (cards and owners) are compatible with the moves in the state. Uses refined-known-have."
  [state assignments]
  (let [cards-per-player      (as-> assignments $
                                (sort-by (fn [x] (str (:owner x))) $)
                                (partition-by :owner $)
                                (map (fn [assignments] {:id    (:owner (first assignments))
                                                        :cards (map :card assignments)}) $))]
    (and
     (every? (fn [assignment] (m-valid-assignment?
                               state
                               assignment))
             assignments)
     (every? (fn [{cards :cards
                   id    :id}] (valid-assignments-to-player? state
                                                             id
                                                             cards))
             cards-per-player))))

(defn valid-placements-of-unknown-owner-cards
  "Return the list of valid possible assignments of cards for which we do not know the owner."
  [state]
  (let [permutations-of-owners (-> (free-owners-to-fill state)
                                   (clojure.math.combinatorics/permutations))
        cards-to-assign        (refined-inferred-unknown-owner state)
        possible-assignments   (map (fn [owners] (map (fn [owner card] {:owner owner
                                                                        :card  card})
                                                      owners
                                                      cards-to-assign)) permutations-of-owners)]
    (filter (fn [assignments] (valid-assignments? state assignments))
            possible-assignments)))

(defn refined-possible-envelopes
  "Try all possible envelopes based on refined-inferred-nobody-has and refined-know-has and remove those which imply any 
   contradictions. For large numbers of possible envelopes this method is very slow."
  [state]
  (let [refined-inferred-nobody-has   (refined-inferred-nobody-has state)
        possible-other-envelope-cards (->> (valid-placements-of-unknown-owner-cards state)
                                           (map (fn [placements]
                                                  (as-> placements $
                                                    (filter (fn [{id :owner}] (= id :envelope)) $)
                                                    (map :card $))))
                                           (map set))]
    (->>
     (cond
       (= 3 (count refined-inferred-nobody-has))   refined-inferred-nobody-has
       (= 3 (count possible-other-envelope-cards)) possible-other-envelope-cards
       :else                                       (for [known                [refined-inferred-nobody-has]
                                                         possible-assignments possible-other-envelope-cards]
                                                     (clojure.set/union known possible-assignments)))
     (map envelope-set-to-sorted-seq))))

(defn number-of-possible-envelopes
  [state]
  (-> (refined-possible-envelopes state)
      (count)))

(defn -main
  "Demo the functionality by printing the number of possible envelopes after a certain number of moves. Takes about 25 seconds."
  [& args]
  (do (dotimes [n 20] (as-> (-> (trim-moves game-one (+ n 20))
                                (number-of-possible-envelopes)) $
                        (println (str "[G1] Number of possible envelopes for " (+ n 20) " moves: " $))))
      (dotimes [n 20] (as-> (-> (trim-moves game-two (+ n 20))
                                (number-of-possible-envelopes)) $
                        (println (str "[G2] Number of possible envelopes for " (+ n 20) " moves: " $))))))