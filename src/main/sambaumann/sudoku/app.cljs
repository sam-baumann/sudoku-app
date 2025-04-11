(ns sambaumann.sudoku.app
  (:require [reagent.dom.client :as rdom]
            [reagent.core :as r]
            [goog.events :as events]))

(defonce root (rdom/create-root (js/document.getElementById "root")))

(defonce active-square (r/atom [-1 -1]))

(defonce board-state (r/atom (into [] (repeat 9 (into [] (repeat 9 0))))))

(defn grid
  "creates the grid"
  []
  [:table
   (into [:tbody]
         (map (fn [i]
                (into [:tr]
                      (map (fn [j]
                             [:td
                              {:className (if (= [i j] @active-square) "cell bg-gray-200" "cell hover:bg-gray-200")
                               :onClick #(reset! active-square [i j])}
                              (let [square-state (get-in @board-state [i j])]
                                (when (not= square-state 0)
                                  square-state))])
                           (range 9))))
              (range 9)))])



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
