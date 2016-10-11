(ns hearts.core)

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
  {:state :play
   :turn 0
   :players (-> names init-players (init-hands deck))
   :trick []})

;; ===== HAND OPERATIONS ===== ;;
(defn has-card? [card hand]
  (if (some #{card} hand) true false))

(defn trick-broken? [])

(defn play-card
  "Each player must follow suit if possible. If a player is void of the suit led, a
  card of any other suit may be discarded. However, if a player has no clubs
  when the first trick is led, a heart or the queen of spades cannot be
  discarded."
  [card hand trick turn])

(defn trick-winner
  "The highest card of the suit led wins a trick and the winner of
  that trick leads next."
  [trick players turn])

;; ===== QUERIES ===== ;;
(defn starting-player [players]
  (let [index-2c (fn [index player]
                   [index (some #{"2C"} (:hand player))])]
    (->> (map-indexed index-2c players)
      (filter #(last %))
      ffirst)))

;; ===== GAMEPLAY OPERATIONS ===== ;;
(defn start-turn [state]
  (assoc state :turn (starting-player (:players state))) 

(defn next-turn [state]
  (let [turn ]
    (->> turn inc (mod 4))))


(comment
  (deal-among 4 (shuffle deck)))
(comment
  (def names ["allan" "adi" "quan" "lucy"])
  (-> names init-players (init-hands deck))
  (def state (init-game-state names))
  (starting-player (:players state))
  (start-turn state))
