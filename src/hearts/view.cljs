(ns hearts.view
  (:require
    [reagent.core :as r]
    [hearts.utils :as utils :refer [spy]]
    [hearts.core :as core]))

(defonce state (r/atom (core/start-game (core/init-game-state ["Allan" "Adi" "Quan" "Lucy"]))))

(defn play-move [card]
  (try
    (doto state
      (swap! core/play-turn card)
      (swap! dissoc :exception))
    (catch js/Object e
      (swap! state assoc :exception e))))

(add-watch state :lifecycle
           (fn [_ _ old-state new-state]
             (when (core/game-over? new-state)
               (core/game-winner new-state))))

(enable-console-print!)

(def scale 1)
(def card-width (* 80 scale))
(def card-height (* 120 scale))
(def board-size (* 600 scale))

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

(defn card
  ([c]
   [card {} c])
  ([opts c]
   (let [default {:src (card->svg c)
                  :style {:width card-width
                          :height card-height
                          :position :absolute}
                  :on-click #(play-move c)}]
     [:img (merge-with conj default opts)])))

(defn hand
  ([cards]
   [hand {} cards])
  ([opts cards]
   (let [offset #(identity {:style {:left (* (/ card-width 5) %)}})
         default-style {:style {:height card-height
                                :position :relative}}]
     (into [:div (merge-with conj default-style opts)]
           (map-indexed #(identity [card (offset %1) %2]) cards)))))

(defn trick
  ([cards]
   [trick {} cards])
  ([opts {:keys [N E S W] :as cards}]
   (let [separation card-width
         -separation (- separation)
         x-offset (/ card-height 4)
         style-map {N {:class "rot-180"
                       :style {:top -separation}}
                    W {:class "rot-90"
                       :style {:left -separation}}
                    E {:class "rot-90"
                       :style {:left separation}}
                    S {:style {:top separation}}}
         items (remove #(nil? (key %)) style-map)
         default {:style {:height (* 2 card-height)
                          :position :relative}}]
     (when (seq cards)
       (into [:div (merge-with conj default opts)]
             (map #(into [card] (reverse %)) items))))))

(defn one-hand [[_ opt] cards]
  [hand (merge-with conj {:style {:position :absolute}} opt) cards])

(defn center-trick [cards]
  (let [offset #(-> board-size (- %) (/ 2))]
    [trick {:style {:top (offset card-height)
                    :right (- (offset card-width))
                    :pointer-events :none}}
     cards]))

(defn score [index player]
  [:div {:style {:display :inline-block
                 :width (/ board-size 4)}}
   [:h3 "Player " (inc index) ": " (:name player)]
   [:p "Score: " (+ (core/player-score player) (:score player))]])

(defn game [state]
  (fn [state]
   (let [{:keys [ntrick turn players trick exception]} @state
        positions (zipmap [:S :E :N :W] players)
        hands (map :hand players)
        rot-x-offset (/ card-height 2)
        rot-y-offset (-> board-size (- (/ card-height 2)))
        dir-order [:S :E :N :W]
        dir-opts {:S {:style {:bottom 0}}
                  :E {:class "rot-270"
                      :style {:right rot-x-offset
                              :top rot-y-offset}}
                  :N {:class "rot-180"
                      :style {:left board-size}}
                  :W {:class "rot-90"
                      :style {:left rot-x-offset
                              :bottom rot-y-offset}}}
        n-players (count players)
        start-dir (- n-players (mod (- ntrick turn) n-players))]
    [:div
     [:h1 "Player " (inc turn) "'s move (" (nth dir-order turn) ")"]
     [:h3 {:style {:color :red}}
      (when exception (.-message exception))]
     [:div {:style {:width board-size
                    :height board-size
                    :background-color "#265C33"
                    :position :relative}}
      (into [:div] (mapv one-hand dir-opts hands))
      [center-trick (zipmap (drop start-dir (cycle dir-order)) trick)]]
     (into [:div] (map-indexed score players))])))

