(ns hearts.core
  (:require [cljs.core.match :refer-macros [match]]
            [hearts.utils :as utils :refer [spy]]
            [cljs.pprint :refer [pprint]]))

;; ===== CREATE DATA ===== ;;
(def suits #{"H" "D" "S" "C"})
(def ranks (concat (map str (range 2 10)) ["T" "J" "Q" "K" "A"]))

(def deck (for [suit suits rank ranks]
            (str rank suit)))

(def max-score 26)

(def card-scores
  (let [hearts (for [rank ranks]
                 (str rank "H"))]
    (-> hearts
      (zipmap (repeat 1))
      (assoc "QS" 13))))

(def has-some? (comp some? some))

;; ===== DECK OPERATIONS ===== ;;
(defn discard
  "removes set of cards from deck"
  [cards deck]
  (remove cards deck))

(defn deal-among
  "returns n hands with the same number of cards"
  [n deck]
  (let [hand-size (quot (count deck) n)]
   (partition hand-size (shuffle deck))))

;; ===== CARD OPERATIONS ===== ;;
(defn card->suit [[rank suit]]
  suit)

(defn card->rank [[rank suit]]
  rank)

;; ===== CONSTRUCT GAME STATE ===== ;;
(defn make-player [-name]
  {:name -name :hand [] :taken [] :score 0})

(defn init-players [names]
  (mapv make-player names))

(defn init-hands [players deck]
  (let [n-players (count players)
        hands (deal-among n-players deck)]
   (mapv #(assoc %1 :hand %2) players hands)))

;; {:ntrick 0
;;  :turn 0 ;; player index. increment mod 4 or change when someone wins trick
;;  :players [{:name "str"
;;             :hand []
;;             :taken []
;;             :score 0}]
;;  :trick []
;;  }
(defn init-game-state [names]
  {:ntrick 0
   :turn 0
   :players (-> names init-players (init-hands deck))
   :trick []})

;; ===== HAND OPERATIONS ===== ;;
(defn has-card? [card hand]
  (if (some #{card} hand) true false))

(defn trick-broken? [players]
  (some (set (keys card-scores)) (mapcat :taken players)))

(defn trick-suit [trick]
  (when-not (empty? trick)
    (card->suit (first trick))))

(defn valid-move
  "Each player must follow suit if possible. If a player is void of the suit led, a
  card of any other suit may be discarded. However, if a player has no clubs
  when the first trick is led, a heart or the queen of spades cannot be
  discarded."
  [card hand trick first?]
  (let [suit #{(trick-suit trick)}
        has-card? (has-some? #{card} hand)
        has-suit? (has-some? #(contains? suit (card->suit %)) hand)
        follows-suit? (contains? suit (card->suit card))]
    (if has-card?
      (match [has-suit? follows-suit? first?]
             [true true _] card
             [true false _] (js/Error. "Card does not match suit.")
             [false _ true] (if (has-some? #{card} (keys card-scores))
                              (js/Error. "Cannot discard heart or queen of spades on first trick.")
                              card)
             [false _ false] card)
      (js/Error. (str "You don't have " card " in your hand.")))))

(defn highest-of-suit [suit cards]
  (let [ranked-ranks (zipmap ranks (range))
        highest (->> cards
                  (filter #(contains? #{suit} (card->suit %)))
                  (apply max-key #(ranked-ranks (card->rank %))))]
    (.indexOf cards highest)))

(defn trick-winner
  "The highest card of the suit led wins a trick and the winner of
  that trick leads next."
  [trick leader]
  (let [winning-card (highest-of-suit (trick-suit trick) trick)]
    (-> winning-card
      (+ leader)
      (mod 4))))

(defn starting-player [players]
  (let [arr (map #(some #{"2C"} (:hand %)) players)]
    (.indexOf arr "2C")))

;; ===== QUERIES ===== ;;

(defn trick-over? [{:keys [ntrick turn players trick] :as state}]
  (= (count trick) (count players)))

(defn round-over? [{:keys [ntrick turn players trick] :as state}]
  (= ntrick (count deck)))

(defn player-score [player]
  (->> player
    :taken
    (map card-scores)
    (apply +)))

(defn game-over? [state]
  (>= (apply max (map :score (:players state)))
      max-score))

(defn game-winner [state]
  (apply min-key (into [:score] (:players state))))

;; ===== GAMEPLAY OPERATIONS ===== ;;
(defn start-game [state]
  (assoc state :turn (starting-player (:players state))))

(defn next-turn [state]
  (let [n (count (:players state))]
    (-> state
      (update :ntrick inc)
      (update :turn #(-> % inc (mod n))))))

(defn shoot-moon? [players]
  (.indexOf (map player-score players) (apply + (vals card-scores))))

(defn update-scores [players]
  (spy "huh?"(map #(update % :score + (player-score %)) players)))

(defn moonshot-update-scores [players shooter]
  (as-> players p
    (map #(update % :score + 26) p)
    (update-in p [shooter :score] - 26)))

(defn clear-cards [player]
  (-> player
    (assoc :taken [])
    (assoc :hand [])))

(defn next-round [state]
  (-> state
    (update :players #(let [shooter (shoot-moon? %)]
                        (if (< shooter 0)
                          (update-scores %)
                          (moonshot-update-scores % shooter))))
    (update :players #(map clear-cards %))
    (update :players init-hands deck)
    (assoc :ntrick 0)))

(defn play-card [{:keys [ntrick turn players trick] :as state} card]
  (let [player (get players turn)
        hand (:hand player)
        ;; that's jank...
        _ (when (and (not= card "2C") (= 0 ntrick (count trick)))
            (throw (js/Error. "Must lead with 2C.")))
        move (utils/throw-err (valid-move card hand trick (zero? ntrick)))
        _ (when (and (contains? card-scores card) (not (trick-broken? players))
                     (zero? (count trick)))
            (throw (js/Error. "Cannot lead with H or QS: Hearts not yet broken.")))]
    (-> state
      (update-in [:players turn :hand] #(discard #{%2} %1) move)
      (update-in [:trick] conj move))))

(defn finish-trick [{:keys [ntrick turn players trick] :as state}]
  (let [winner (trick-winner trick turn)]
    (-> state
      (update-in [:players winner :taken] concat trick)
      (assoc :trick [])
      (assoc :turn winner))))

(defn play-turn [state card]
  (let [next-state (-> state
                    (play-card card)
                    next-turn)
        new-round (comp start-game next-round)]
    (cond-> next-state
      (trick-over? next-state) finish-trick
      (round-over? next-state) new-round)))

(comment
  (deal-among 4 (shuffle deck))

  (def names ["allan" "adi" "quan" "lucy"])
  (def players (-> names init-players (init-hands deck)))

  (def state (init-game-state names))
  (starting-player (:players state))
  (def state-1 (start-turn state))
  (next-turn state-1)

  (def card "1S")
  (def hand '("QH" "QS" "1H" "1S"))
  (def trick '("3C" "KC"))
  (def first? false)
  (valid-move "QH" hand '() first?)
  (valid-move card hand trick first?)
  (highest-of-suit-2 "C" '("3C" "6C" "JH" "7C"))
  (trick-winner '("3C" "6C" "JH" "7C") 2)

  (def state (start-game (init-game-state names)))
  (pprint state)
  (pprint
    (-> state
      (play-turn "JS")
      (play-turn "4S")
      (play-turn "KS")
      (play-turn "8S")))

  )
