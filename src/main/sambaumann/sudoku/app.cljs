(ns sambaumann.sudoku.app
  (:require [reagent.dom.client :as rdom]
            [reagent.core :as r]
            [goog.events :as events]))

(defonce root (rdom/create-root (js/document.getElementById "root")))

(defonce active-square (r/atom [-1 -1]))

;the puzzle is loaded in as this atom
(defonce original-state (r/atom (into [] (repeat 9 (into [] (repeat 9 0))))))

;the current digits in the board - 0 if not filled
(defonce board-state (r/atom (into [] (repeat 9 (into [] (repeat 9 0))))))

(defn cell
  "cell object"
  [i j]
  [:div
   ;div properties
   ;styling
   {:className (if (= [i j] @active-square) "cell bg-gray-200" "cell hover:bg-gray-200")
    ; sets the active square when clicked
    :onClick #(reset! active-square [i j])}
   ;if the state is 0 - don't show it, otherwise do
   (let
    [square-state (get-in @board-state [i j])]
     (when
      (not= square-state 0)
       square-state))])

(defn group
  "group of 3x3 cells"
  [starti startj]
  (into [:div {:className "flex border-2"}]
        (map (fn
               [i]
               (into [:div]
                     (map #(cell i %)
                          (range startj (+ startj 3)))))
             (range starti (+ starti 3)))))

(defn grid
  "creates the grid"
  []
  (into [:div {:className "flex border-2 w-min rounded-lg"}]
        (map (fn [i]
               (into [:div]
                     (map (fn [j]
                            (group i j))
                          (range 0 9 3))))
             (range 0 9 3))))



(defn app [] (grid))

(defn keypress-listener
  [event]
  (let [pressed-key event.event_.key]
    (if (contains? (set (range 1 10)) (int pressed-key))
      (swap! board-state assoc-in @active-square (int pressed-key))
      :else)))

(defn ^:export init []
  (events/removeAll js/document "keyup") ; remove all listeners, as an old one may still be listening if we are hot-reloading during dev
  (events/listen js/document "keyup" keypress-listener) ; add input listener
  (rdom/render root [app]) ;render UI
  )