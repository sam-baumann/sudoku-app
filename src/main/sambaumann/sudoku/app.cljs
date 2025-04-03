(ns sambaumann.sudoku.app
  (:require [reagent.dom.client :as rdom]
            [reagent.core :as r]))

(defonce root (rdom/create-root (js/document.getElementById "root")))

(defonce active-square (r/atom [0 0]))

(defonce board-state (r/atom (into [] (repeat 9 (into [] (repeat 9 0))))))

(defn grid
  "creates the grid"
  []
  [:table
   (into [:tbody]
         (map (fn [i]
                (into [:tr]
                      (map (fn [j]
                             [:td {:className "cell"
                                   :onClick #(swap! board-state assoc-in [i j] 5)}
                              (get-in @board-state [i j])])
                           (range 9))))
              (range 9)))])

(defn app []
  (grid))

(defn ^:export init []
  (rdom/render root [app]))
