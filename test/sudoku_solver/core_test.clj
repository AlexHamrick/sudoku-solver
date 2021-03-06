(ns sudoku-solver.core-test
  (:require [clojure.test :refer :all]
            [sudoku-solver.solver :refer :all]
            [sudoku-solver.utils :refer :all]))

(deftest hard-test
  (testing "testing a hard sudoku puzzle"
    (is (= 
         (solve-file "resources/hard.txt" 3 3) 
         [[1 6 2 8 5 7 4 9 3]
          [5 3 4 1 2 9 6 7 8]
          [7 8 9 6 4 3 5 2 1]
          [4 7 5 3 1 2 9 8 6]
          [9 1 3 5 8 6 7 4 2]
          [6 2 8 7 9 4 1 3 5]
          [3 5 6 4 7 8 2 1 9]
          [2 4 1 9 3 5 8 6 7]
          [8 9 7 2 6 1 3 5 4]]))))


(deftest med-test
  (testing "testing a medium sudoku puzzle"
    (is (=
         (solve-file "resources/medium.txt" 3 3)
         [[1 3 2 6 4 9 7 8 5]
          [7 5 8 2 1 3 6 4 9]
          [9 6 4 7 8 5 1 2 3]
          [5 4 3 8 9 7 2 1 6]
          [2 7 6 5 3 1 8 9 4]
          [8 9 1 4 2 6 5 3 7]
          [6 1 9 3 7 8 4 5 2]
          [3 2 7 1 5 4 9 6 8]
          [4 8 5 9 6 2 3 7 1]]))))

(deftest easy-test
  (testing "testing an easy sudoku puzzle"
    (is (=
         (solve-file "resources/easy.txt" 3 3)
         [[4 9 5 6 1 8 2 3 7]
          [7 2 6 3 4 9 5 8 1]
          [3 1 8 7 2 5 4 6 9]
          [5 7 2 8 9 4 3 1 6]
          [6 4 3 5 7 1 8 9 2]
          [1 8 9 2 6 3 7 4 5]
          [8 3 7 9 5 6 1 2 4]
          [2 6 4 1 8 7 9 5 3]
          [9 5 1 4 3 2 6 7 8]]))))

(deftest off-size-test-3-2
  (testing "testing an easy sudoku puzzle"
    (is (=
         (solve-file "resources/basic3_2.txt" 3 2)
         [[2 5 6 4 3 1] 
          [1 3 4 2 5 6] 
          [5 6 1 3 2 4] 
          [3 4 2 6 1 5] 
          [4 2 5 1 6 3] 
          [6 1 3 5 4 2]]))))

(deftest off-size-test-2-4
  (testing "testing an easy sudoku puzzle"
    (is (=
         (solve-file "resources/basic2_4.txt" 2 4)
         [[2 3 5 6 1 4 7 8]
          [1 4 2 7 6 8 5 3]
          [5 7 1 8 2 3 4 6]
          [6 8 3 4 5 7 1 2]
          [3 1 4 2 7 6 8 5]
          [4 2 7 3 8 5 6 1]
          [7 6 8 5 3 1 2 4]
          [8 5 6 1 4 2 3 7]]))))