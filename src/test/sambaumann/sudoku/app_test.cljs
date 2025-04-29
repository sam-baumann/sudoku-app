(ns sambaumann.sudoku.app-test
  (:require
   [cljs.test :refer [deftest is testing]]
   [sambaumann.sudoku.app :refer
    [blank-puzzle get-col get-group get-row solve-puzzle is-full all-unique is-solved group-from-coords valid-entry]]))


(deftest test-blank-puzzle
  (testing "Returns a 9x9 grid of 0s"
    (let [p (blank-puzzle)]
      (is (= 9 (count p)))
      (is (every? #(= 9 (count %)) p))
      (is (every? #(every? zero? %) p)))))

(def sample-puzzle
  [[1 2 3 4 5 6 7 8 9]
   [4 5 6 7 8 9 1 2 3]
   [7 8 9 1 2 3 4 5 6]
   [2 3 4 5 6 7 8 9 1]
   [5 6 7 8 9 1 2 3 4]
   [8 9 1 2 3 4 5 6 7]
   [3 4 5 6 7 8 9 1 2]
   [6 7 8 9 1 2 3 4 5]
   [9 1 2 3 4 5 6 7 8]])

(deftest get-row-test
  (is (= (get-row sample-puzzle 0) [1 2 3 4 5 6 7 8 9]))
  (is (= (get-row sample-puzzle 4) [5 6 7 8 9 1 2 3 4]))
  (is (= (get-row sample-puzzle 8) [9 1 2 3 4 5 6 7 8])))

(deftest get-col-test
  (is (= (get-col sample-puzzle 0) [1 4 7 2 5 8 3 6 9]))
  (is (= (get-col sample-puzzle 3) [4 7 1 5 8 2 6 9 3]))
  (is (= (get-col sample-puzzle 8) [9 3 6 1 4 7 2 5 8])))

(deftest get-group-test
  (is (= (get-group sample-puzzle 0)
         [1 2 3 4 5 6 7 8 9]))
  (is (= (get-group sample-puzzle 4)
         [5 6 7 8 9 1 2 3 4]))
  (is (= (get-group sample-puzzle 8)
         [9 1 2 3 4 5 6 7 8])))

(deftest is-full-test
  (is (= (is-full [1 2 3 4 5 6 7 8 9]) true))
  (is (= (is-full [0 1 0 2 3 4 5 6 7]) false)))

(deftest all-unique-test
  (is (= (all-unique [1 2 3 4 5 6 7 8 9]) true))
  (is (= (all-unique [1 1 2 3 4 5 6 6 7]) false)))

(deftest test-is-solved
  (is (= (is-solved (blank-puzzle)) false))
  (is (= (is-solved sample-puzzle) true)))

(deftest test-group-from-coords
  (is (= (group-from-coords 0 0) 0))
  (is (= (group-from-coords 8 8) 8))
  (is (= (group-from-coords 4 0) 3))
  (is (= (group-from-coords 2 6) 2)))

(deftest test-valid-entry
  (is (= (valid-entry (blank-puzzle) 1 0 0) true))
  (is (= (valid-entry (assoc-in (blank-puzzle) [0 0] 1) 1 0 1) false)))

(deftest test-solve-puzzle
  (testing "Solve puzzle"
    (is (= (first (solve-puzzle (-> sample-puzzle (assoc-in [0 0] 0) (assoc-in [5 5] 0) (assoc-in [6 6] 0) (assoc-in [6 7] 0)))) sample-puzzle))))