(ns sambaumann.sudoku.app
  (:require [reagent.dom.client :as rdom]
            [reagent.core :as r]
            [goog.events :as events]))

;forward declare atoms used in the game logic and UI logic. 
(declare active-square board-state original-state blank-puzzle)

;Game Logic
(defn check-puzzle
  ;checks the puzzle against the known correct, returns true or false and updates the error cells atom
  []
  false)

(defn check-row
  ;checks the individual row
  [puzzle row-index]
  false)

(defn check-col
  ;check column for correctness
  [puzzle col-index]
  (= (get-in puzzle col-index) (set (range 1 10))))

(defn solve-puzzle
  ;returns solution to the given puzzle
  [puzzle]
  (blank-puzzle))

(defn blank-puzzle
  []
  (into [] (repeat 9 (into [] (repeat 9 0)))))

(defn set-puzzle []
  (reset! original-state [[0 0 0 2 6 0 7 0 1]
                          [6 8 0 0 7 0 0 9 0]
                          [1 9 0 0 0 4 5 0 0]
                          [8 2 0 1 0 0 0 4 0]
                          [0 0 4 6 0 2 9 0 0]
                          [0 5 0 0 0 3 0 2 8]
                          [0 0 9 3 0 0 0 7 4]
                          [0 4 0 0 5 0 0 3 6]
                          [7 0 3 0 1 8 0 0 0]]))

;UI Componenets
(defn cell
  "cell object"
  [i j]
  (let [active-square? (= [i j] @active-square)
        in-original? (not= 0 (get-in @original-state [i j]))]
    [:div
   ;div properties
   ;styling
     {:className (str "cell" (cond
                               in-original? " bg-gray-200"
                               active-square? " bg-yellow-100"
                               :else " hover:bg-gray-200"))
    ; sets the active square when clicked
      :onClick #(when (not in-original?) (reset! active-square [i j]))}
   ;if the state is 0 - don't show it, otherwise do
     (let
      [square-state (get-in @board-state [i j])]
       (when
        (not= square-state 0)
         square-state))]))

(defn group
  "group of 3x3 cells"
  [starti startj]
  (into [:div {:className "border-2"}]
        (map (fn
               [i]
               (into [:div {:className "flex"}]
                     (map #(cell i %)
                          (range startj (+ startj 3)))))
             (range starti (+ starti 3)))))

(defn grid
  "creates the grid"
  []
  (into [:div {:className "border-2 w-min rounded-lg"}]
        (map (fn [i]
               (into [:div {:className "flex"}]
                     (map (fn [j]
                            (group i j))
                          (range 0 9 3))))
             (range 0 9 3))))

(defn app []
  [:div
   (grid)
   [:button {:onClick set-puzzle
             :className "px-4 py-2 rounded-md border hover:bg-blue-100"}
    "reset puzzle"]])

;Event Listeners
(defn keypress-listener
  [event]
  (let [pressed-key event.event_.key]
    (cond
      (contains? (set (range 1 10)) (int pressed-key))
      (swap! board-state assoc-in @active-square (int pressed-key))
      (contains? #{"Backspace" "Delete"} pressed-key)
      (swap! board-state assoc-in @active-square 0)
      :else (js/console.log pressed-key))))

;Atoms
(defonce root (rdom/create-root (js/document.getElementById "root")))

(defonce active-square (r/atom [-1 -1]))

;the current digits in the board - 0 if not filled
(defonce board-state (r/atom (blank-puzzle)))

;the puzzle is loaded in as this atom 
(defonce original-state (r/atom (blank-puzzle)))
;when the puzzle is changed, clear the board
(add-watch original-state :board-clear
           #(reset! board-state @original-state))

;Initialization function (called on startup)
(defn ^:export init []
  (events/removeAll js/document "keyup") ; remove all listeners, as an old one may still be listening if we are hot-reloading during dev
  (events/listen js/document "keyup" keypress-listener) ; add input listener
  (rdom/render root [app]) ;render UI
  )