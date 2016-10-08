(ns hearts.core)

;; ===== CREATE DATA ===== ;;
(def suits #{"H" "D" "S" "C"})
(def ranks (concat (map str (range 2 10)) ["T" "J" "Q" "K" "A"]))

(def deck (for [suit suits rank ranks]
            (str rank suit)))

(defn card-scores []
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


;; ===== SETUP GAME STATE ===== ;;
{:state :play
 :turn 0 ;; player index. increment mod 4 or change when someone wins trick
 :players [{:hand []
            :taken []
            :score 0}]
 :trick []
 }
