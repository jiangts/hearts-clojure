(ns hearts.dev.cards
  (:require
    [devcards.core]
    [reagent.core :as r]
    [hearts.core :as core]
    [hearts.view :as view])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-doc defcard-rg]]))

(enable-console-print!)

(def card-width 100)
(def card-height 150)

(def rank->name
  (let [numbers (map str (range 2 10))]
    (merge {"T" "10"
            "A" "ace"
            "J" "jack"
            "Q" "queen"
            "K" "king"}
           (zipmap numbers numbers))))

(def suit->name {"D" "diamonds"
                 "H" "hearts"
                 "S" "spades"
                 "C" "clubs"})

(defn card->svg [card]
  (if (= card "XX")
    "img/cards/card_back.svg"
    (let [[rank suit] card]
      (str "img/cards/" (rank->name rank) "_of_" (suit->name suit) ".svg"))))

(defn card [card offset]
  [:img {:src (card->svg card)
         :style {:width card-width
                 :height card-height
                 :position :absolute
                 :left offset}
         }])

(defn hand
  ([cards]
   [hand {} cards])
  ([opts cards]
   (into [:div (merge {:style {:height card-height}} opts)]
         (map-indexed #(identity [card %2 (* (/ card-width 5) %1)]) @cards))))


(defcard-doc
  "
# Hearts Rules

### THE PACK
  The standard 52-card pack is used.

### OBJECTIVE
  To be the player with the lowest score at the end of the
  game. When one player hits the agreed-upon score or higher, the game ends; and
  the player with the lowest score wins.

### CARD VALUES/SCORING
  At the end of each hand, players count the number of
  hearts they have taken as well as the queen of spades, if applicable. Hearts
  count as one point each and the queen counts 13 points.  Each heart - 1 point
  The Q - 13 points The aggregate total of all scores for each hand must be a
  multiple of 26.  The game is usually played to 100 points (some play to 50).
  When a player takes all 13 hearts and the queen of spades in one hand, instead
  of losing 26 points, that player scores zero and each of his opponents score
  an additional 26 points.

### THE DEAL
  Deal the cards one at a time, face down, clockwise. In a
  four-player game, each is dealt 13 cards; in a three-player game, the 2 of
  diamonds should be removed, and each player gets 17 cards; in a five-player
  game, the 2 of diamonds and 2 of clubs should be removed so that each player
  will get 10 cards.

### THE PLAY
  The player holding the 2 of clubs after the pass makes the opening
  lead. If the 2 has been removed for the three handed game, then the 3 of clubs
  is led.

Each player must follow suit if possible. If a player is void of the suit led, a
  card of any other suit may be discarded. However, if a player has no clubs
  when the first trick is led, a heart or the queen of spades cannot be
  discarded. The highest card of the suit led wins a trick and the winner of
  that trick leads next. There is no trump suit.

The winner of the trick collects it and places it face down. Hearts may not be
  led until a heart or the queen of spades has been discarded. The queen does
  not have to be discarded at the first opportunity.

The queen can be led at any time.

Source: http://www.bicyclecards.com/how-to-play/hearts
")


(defcard-rg hand
  (fn [cards]
    [hand cards])
  (r/atom (first (core/deal-among 4 (shuffle core/deck))))
  {:inspect-data true})

(defcard-rg card
  [hand (r/atom ["QS"])])

(defcard-rg card-back
  [hand (r/atom ["XX"])])

(defcard-rg card-rot-90
  [hand {:class "rot-90"} (r/atom ["XX"])])

(defcard-rg card-rot-180
  [hand {:class "rot-180"} (r/atom ["XX"])])

(defcard-rg card-rot-270
  [hand {:class "rot-270"} (r/atom ["XX"])])

(defcard-rg hand
  (fn [cards]
    [hand {:class "rot-270"} cards])
  (r/atom (map #(identity "XX") (range 13)))
  {:inspect-data true})

