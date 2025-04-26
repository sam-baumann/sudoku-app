(ns sambaumann.sudoku.app-test
  (:require
   [cljs.test :refer [deftest is testing]]
   [sambaumann.sudoku.app :refer [blank-puzzle get-col get-group get-row solve-puzzle]]))


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

(deftest test-solve-puzzle
  (testing "Currently just returns blank puzzle"
    (is (= (blank-puzzle) (solve-puzzle (blank-puzzle))))))