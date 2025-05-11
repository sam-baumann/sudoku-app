(ns sambaumann.sudoku.app
  (:require [reagent.dom.client :as rdom]
            [reagent.core :as r]
            [goog.events :as events]
            [clojure.set :as set]
            [clojure.string :as str]))

;forward declare atoms used in the game logic and UI logic. 
(declare active-square board-state original-state blank-puzzle)

;forward declare funcitons for solving so they can be used in calls to each other
(declare fill eliminate vec2grid grid2vec)

;Game Logic
;reimplement peter norvig's sudoku solver in clojurescript; mine wasn't efficient enough

(defn cross
  [A B]
  (for [a A
        b B]
    (+ a b)))

(def digits "123456789")

(def rows "ABCDEFGHI")

(def cols digits)

(def squares (cross rows cols))

(def all-boxes
  (for [rs ["ABC" "DEF" "GHI"]
        cs ["123" "456" "789"]]
    (cross rs cs)))

(def all-units
  (concat (for [c cols] (set (cross rows c)))
          (for [r rows] (set (cross r cols)))
          (map set all-boxes)))

(def units
  (into {} (for [s squares]
             [s (for [u all-units
                      :when (contains? u s)] u)])))

(def peers
  (into {} (for [s squares]
             [s (disj (apply set/union (get units s)) s)])))

(defn is-solved
  [solution]
  (and solution
       (every? true? (for [unit all-units]
                       (= (set (for [s unit] (get solution s))) (set digits))))))

(defn is-solution
  [solution puzzle]
  (and solution ;solution is not none
       (every? true? (for [s squares]
                       (str/includes? (get puzzle s) (get solution s)))) ;ensure solution fits in the original puzzle
       (is-solved solution)))

(defn constrain
  ;propogate constraints on a copy of grid to yield a new constrained grid
  [grid]
  (let [result (into {} (for [s squares] [s digits]))]
    (reduce (fn [res s]
              (if (and res (= 1 (count (grid s))))
                (fill res s (grid s))
                res))
            result
            (keys grid))))

(defn fill
  [grid s d]
  (if (= (grid s) d)
    grid
    (reduce (fn [res d2] (when res (eliminate res s d2)))
            grid
            (filter #(not= % d)
                    (grid s)))))

(defn eliminate
  [grid s d]
  (if (not (str/includes? (grid s) d))
    grid ; already eliminated 
    (let [new-grid (update-in grid [s] str/replace d "")
          after-peers ;apply peer rules
          (cond (empty? (new-grid s))
                nil ;if the square is empty, return nil
                (= 1 (count (new-grid s))) ;if there's only one possibility, eliminate it from the peers
                (reduce (fn [ret s2] (if (not ret)
                                       nil
                                       (eliminate ret s2 (new-grid s))))
                        new-grid (peers s))
                :else new-grid)] ;otherwise, just go with the original grid
      (reduce (fn [ret unit]
                (when ret
                  (let [dplaces (filter #(str/includes? (ret %) d) unit)]
                    (cond
                      (= 0 (count dplaces)) nil
                      (= 1 (count dplaces)) (fill ret (first dplaces) d)
                      :else ret))))
              after-peers (units s)))))

(defn search
  [grid]
  (when grid
    (let [min-count (apply min (filter #(> % 1) (map #(count (grid %)) squares)))
          check-square (shuffle (filter #(= min-count (count (grid %))) squares))] ;we shuffle this to provide some randomness - does not have an impact on solving but helps create random new puzzles using this function
      (if (= 0 (count check-square)) (list grid) ;no squares with multiple possibilities - search succeeded
          (lazy-seq
           (mapcat (fn [d] (search (fill grid (first check-square) d)))
                   (grid (first check-square))))))))

;a 'grid' is a map where keys are the 'squares' set, and vals are possible digits.
;a 'vec' is a 9x9 int vector representation of the puzzle, used primarily for storing/displaying the puzzle
;next two functions convert between them
(defn vec2grid
  [vec]
  (into {}
        (for [i (range (count squares))]
          (let [row (quot i 9)
                col (mod i 9)
                vec-val (get-in vec [row col])
                grid-val (if (= vec-val 0) digits
                             (str vec-val))]
            [(nth squares i) grid-val]))))

(defn grid2vec
  [grid]
  (vec
   (for [row (range 9)]
     (vec
      (for [col (range 9)]
        (let [i (+ (* row 9) col)
              cur-square (nth squares i)
              grid-val (grid cur-square)]
          (if (= 1 (count grid-val)) (int grid-val)
              0)))))))

(defn blank-puzzle-vec
  []
  (into [] (repeat 9 (into [] (repeat 9 0)))))

(defn blank-puzzle-grid
  []
  (vec2grid (blank-puzzle-vec)))

(defn solve-puzzle
  [puzzle]
  (first (search (constrain puzzle))))

(defn unique-solution?
  [puzzle]
  (= 1 (count (take 2 (search (constrain puzzle))))))

(defn new-filled-puzzle
  []
  (first (search (blank-puzzle-grid))))

(defn filled-squares
  [puzzle]
  (filter #(= 1 (count (puzzle %))) squares))

(defn create-unsolved-puzzle
  ;remove random squares until puzzle is unsolvable.
  [puzzle spaces-to-remove]
  (cond
    (not (unique-solution? puzzle)) nil ;base case - if the puzzle does not have a unique solution, return nil
    (= 0 spaces-to-remove) puzzle
    :else (let [candidates (shuffle (filled-squares puzzle))]
            (some (fn [remove-candidate]
                    (let [next-puzzle (assoc puzzle remove-candidate digits)]
                      (create-unsolved-puzzle next-puzzle (dec spaces-to-remove))))
                  candidates))))

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

(defn new-puzzle-creator
  "create a new puzzle based on a difficulty slider"
  []
  [:button {:onClick #(reset! original-state (grid2vec (create-unsolved-puzzle (new-filled-puzzle) 50)))
            :className "px-4 py-2 rounded-md border hover:bg-blue-100"}
   "New Puzzle"])

(defn app []
  [:div
   (grid)
   (new-puzzle-creator)])

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
(defonce root (delay (rdom/create-root (js/document.getElementById "root"))))

(defonce active-square (r/atom [-1 -1]))

;the current digits in the board - 0 if not filled
(defonce board-state (r/atom (blank-puzzle-vec)))

;the puzzle is loaded in as this atom 
(defonce original-state (r/atom (blank-puzzle-vec)))
;when the puzzle is changed, clear the board
(add-watch original-state :board-clear
           #(reset! board-state @original-state))

;Initialization function (called on startup)
(defn ^:export init []
  (events/removeAll js/document "keyup") ; remove all listeners, as an old one may still be listening if we are hot-reloading during dev
  (events/listen js/document "keyup" keypress-listener) ; add input listener
  (rdom/render @root [app]) ;render UI
  )