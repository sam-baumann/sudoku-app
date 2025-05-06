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

(defn get-row
  ;checks the individual row
  [puzzle row-index]
  (get-in puzzle [row-index]))

(defn get-col
  ;check column for correctness
  [puzzle col-index]
  (map #(get % col-index) puzzle))

(defn get-group
  [puzzle group-index]
  (let [starti (* 3 (quot group-index 3))
        startj (* 3 (mod group-index 3))]
    (apply concat
           (map
            (fn [i] (map (fn [j]
                           (get-in puzzle [i j]))
                         (range startj (+ 3 startj))))
            (range starti (+ 3 starti))))))

(defn is-full
  [puzzle-component]
  ; a row is full if it has no zeroes
  (= 0 (count (filter zero? puzzle-component))))

(defn all-unique
  [puzzle-component]
  (let
   [filled-entries (remove zero? puzzle-component)]
    (= (distinct filled-entries) filled-entries)))

(defn is-solved
  [puzzle]
  ;ensure all rows,cols, and groups are fully filled and unique
  (every? true?
          (map
           (fn [i] (every? true?
                           (map #(every? true?
                                         (list (is-full %) (all-unique %)))
                                (list (get-row puzzle i) (get-col puzzle i) (get-group puzzle i)))))
           (range 9))))

(defn group-from-coords
  [i j]
  (+ (* 3 (quot i 3)) (quot j 3)))

(defn valid-entry
  ;returns wether inserting value into puzzle at [i j] is valid
  [puzzle value i j]
  (let
   [new-puzzle (assoc-in puzzle [i j] value)]
    (every? true? (map all-unique [(get-row new-puzzle i) (get-col new-puzzle j) (get-group new-puzzle (group-from-coords i j))]))))

(defn all-pairs
  []
  (apply concat (map (fn [i] (map (fn [j] [i j]) (range 9))) (range 9))))

(defn empty-cells
  [puzzle]
  (filter #(= 0 (get-in puzzle %)) (all-pairs)))

(defn constraints
  [puzzle]
  ;return a vector of vector of sets representing the valid next states of the given puzzle
  (into [] (map (fn [i] (into [] (map (fn [j] (set (filter #(valid-entry puzzle % i j) (range 1 10)))) (range 9)))) (range 9))))

(defn count-constraints
  [puzzle constraint]
  (remove nil?
          (apply concat
                 (map (fn [i] (map (fn [j] (if (= (get-in puzzle [i j]) 0)
                                             [i j (count (get-in constraint [i j]))]
                                             nil)) (range 9))) (range 9)))))

(defn most-constrained-cells
  ;takes in constraints, and returns the positions and possible values of the most constrained cells
  ;(only unfilled ones)
  [puzzle constraint]
  (let [constraint-counts (count-constraints puzzle constraint)
        min-count (apply min (map #(nth % 2) constraint-counts))]
    (filter #(= (nth % 2) min-count) constraint-counts)))

(defn next-puzzles
  [puzzle constraint]
  (map #(vector (first %) (second %) (nth % 2) (assoc-in puzzle [(first %) (second %)] (nth % 2)))
       (mapcat #(map (fn [i] [(first %) (second %) i]) (get-in constraint [(first %) (second %)])) (shuffle (most-constrained-cells puzzle constraint)))))

(defn reduce-constraint
  [i j val constraint]
  (let [group_starti (* 3 (quot i 3))
        group_startj (* 3 (quot j 3))
        coords-to-edit (set (concat
                             (for [x (range 9)] [i x])
                             (for [x (range 9)] [x j])
                             (for [x (range group_starti (+ 3 group_starti))
                                   y (range group_startj (+ 3 group_startj))] [x y])))]
    (reduce (fn [cons coord] (update-in cons coord disj val)) constraint coords-to-edit)))

(defn check-each-valid
  ;ensure each cell still has a valid value
  [puzzle constraints]
  (not (some #(and (= (get-in puzzle %) 0)
                   (= (count (get-in constraints %)) 0))
             (for [x (range 9)
                   y (range 9)]
               [x y]))))

(defn recursive-solve-puzzle
  ;returns solution to the given puzzle 
  [puzzle blanks visited constraint]
  ;if already visited, back out
  (lazy-seq
   (if (@visited puzzle) []
       (do (swap! visited conj puzzle)
           (cond
             (= blanks 0) [puzzle]
             (not (check-each-valid puzzle constraint)) []
             :else (mapcat (fn [next-puzzle] (recursive-solve-puzzle
                                              (nth next-puzzle 3)
                                              (- blanks 1)
                                              visited
                                              (reduce-constraint (first next-puzzle) (second next-puzzle) (nth next-puzzle 2) constraint)))
                           (next-puzzles puzzle constraint)))))))

(defn solve-puzzle
  [puzzle]
  (recursive-solve-puzzle puzzle (count (empty-cells puzzle)) (atom #{}) (constraints puzzle)))

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
(defonce root (delay (rdom/create-root (js/document.getElementById "root"))))

(defonce active-square (r/atom [-1 -1]))

;the current digits in the board - 0 if not filled
(defonce board-state (r/atom (blank-puzzle)))
(add-watch board-state :check-solved
           (fn [] (when (is-solved @board-state) (js/console.log "solved!"))))

;the puzzle is loaded in as this atom 
(defonce original-state (r/atom (blank-puzzle)))
;when the puzzle is changed, clear the board
(add-watch original-state :board-clear
           #(reset! board-state @original-state))

;Initialization function (called on startup)
(defn ^:export init []
  (events/removeAll js/document "keyup") ; remove all listeners, as an old one may still be listening if we are hot-reloading during dev
  (events/listen js/document "keyup" keypress-listener) ; add input listener
  (rdom/render @root [app]) ;render UI
  )