(ns sambaumann.sudoku.app
  (:require [reagent.dom.client :as rdom]))

(defonce root (rdom/create-root (js/document.getElementById "root")))

(defn app []
  [:div "test1"])

(defn ^:export init [] 
  (rdom/render root (app)))
