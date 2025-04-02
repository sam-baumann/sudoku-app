(ns sambaumann.sudoku.app
  (:require [reagent.dom.client :as rdom]))

(defonce root (rdom/create-root (js/document.getElementById "root")))

(defn grid
  "creates the grid"
  []
  [:table
   (into [:tbody]
         (map (fn [i]
                (into [:tr]
                      (map (fn [j]
                             [:td {:className "cell"
                                   :onClick #(js/alert
                                              (str "Cell " i ", " j " clicked"))}
                              \" i j \"])
                           (range 9))))
              (range 9)))])

(defn app []
  (grid))

(defn ^:export init []
  (rdom/render root (app)))
