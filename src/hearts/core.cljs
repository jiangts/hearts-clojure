(ns hearts.core
  (:require [cljs.core.match :refer-macros [match]]))

;; ===== CREATE DATA ===== ;;
(def suits #{"H" "D" "S" "C"})
(def ranks (concat (map str (range 2 10)) ["T" "J" "Q" "K" "A"]))

(def deck (for [suit suits rank ranks]
            (str rank suit)))

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
  (partition (quot (count deck) n) deck))

(defn draw
  "returns n cards drawn from deck + remainder of deck"
  [n deck]
  (split-at n deck))

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

;; {:state :play
;;  :turn 0 ;; player index. increment mod 4 or change when someone wins trick
;;  :players [{:name "str"
;;             :hand []
;;             :taken []
;;             :score 0}]
;;  :trick []
;;  }
(defn init-game-state [names]
  {:state :first
   :turn 0
   :players (-> names init-players (init-hands deck))
   :trick []})

;; ===== HAND OPERATIONS ===== ;;
(defn has-card? [card hand]
  (if (some #{card} hand) true false))

#_(defn trick-broken? [])  ;; not in this version!

(defn trick-suit [trick]
  (if (empty? trick)
    suits
    #{(card->suit (first trick))}))

(defn play-card
  "Each player must follow suit if possible. If a player is void of the suit led, a
  card of any other suit may be discarded. However, if a player has no clubs
  when the first trick is led, a heart or the queen of spades cannot be
  discarded."
  [card hand trick first?]
  (let [suit (trick-suit trick)
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
  (let [ranked-ranks (zipmap (range) ranks)]
    (->> cards
      (filter #(contains? suit (card->suit %)))
      (sort-by #(get ranked-ranks (int (first %))))
      last)))

(defn trick-winner
  "The highest card of the suit led wins a trick and the winner of
  that trick leads next."
  [trick leader]
  (let [suit (trick-suit trick)
        winning-card (highest-of-suit suit trick)
        player-idx (fn [idx card]
                     [(mod idx 4) card])]
    (->> trick
      (map player-idx (drop leader (range)))
      (filter #(= winning-card (last %)))
      ffirst)))

;; ===== QUERIES ===== ;;
(defn starting-player [players]
  (let [index-2c (fn [index player]
                   [index (some #{"2C"} (:hand player))])]
    (->> players  ;; look familiar?
      (map-indexed index-2c)
      (filter #(last %))
      ffirst)))

;; ===== GAMEPLAY OPERATIONS ===== ;;
(defn start-turn [state]
  (assoc state :turn (starting-player (:players state))))

(defn next-turn [state]
  (let [n (count (:players state))]
    (-> state
      (assoc :state :play)  ;; not :first trick
      (update :turn #(-> % inc (mod n))))))

(comment
  (deal-among 4 (shuffle deck))

  (def names ["allan" "adi" "quan" "lucy"])
  (-> names init-players (init-hands deck))

  (def state (init-game-state names))
  (starting-player (:players state))
  (def state-1 (start-turn state))
  (next-turn state-1)

  (def card "1S")
  (def hand '("QH" "QS" "1H" "1S"))
  (def trick '("3C" "KC"))
  (def first? true)
  (play-card card hand trick first?)
  (trick-winner '("3C" "6C" "JH" "7C") 2)
  )
