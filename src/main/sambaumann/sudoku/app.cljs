(ns sambaumann.sudoku.app
  (:require [reagent.dom.client :as rdom]))

(defonce root (rdom/create-root (js/document.getElementById "root")))

(defn grid
  "creates the grid"
  []
  [:table (into [:tbody] (map #(vector :tr [:td %]) (range 9)))])

(defn app []
  (grid))

(defn ^:export init []
  (rdom/render root (app)))
