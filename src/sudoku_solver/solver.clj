(ns sudoku-solver.solver)
;; using [net.mikera/core.matrix "0.18.0"] as a dependency
(require '[clojure.core.matrix :as matrix])
; (require '[clojure.string :as str])

(declare recsolve
         try-vals
         valid-number?
         available-nums
         box-row-index
         box-col-index
         same-row?
         same-col?
         same-box?
         overlap?
         try-rand-vals
         replace-random)

;; Solves a sudoku board passed with the dimensions
;; @param board A vector of numbers representing a sudoku
;;   board to be solved
;; @param rows The number of rows in the board
;; @param cols The number of cols in the board
(defn solve
  [board rows cols]
  (let
   [nums (* rows cols)
    res (recsolve (vec (flatten board)) rows cols 0 nums)]
    (matrix/reshape res [nums nums])))

;; A recursive method that solves a sudoku board-
;; It steps through each cell and tries each possible number
;; (assuming that number would be valid). If it reaches
;; a point where none of the attempted numbers are valid, it 
;; backtracks to previous cells
;; @param board A vector of numbers representing a sudoku
;;   board to be solved
;; @param rows The number of rows in the board
;; @param cols The number of cols in the board
;; @param idx The index for which we are plugging in a value
;; @param nums the number of values that exist on the board
;; - for a normal 3x3 board, this would be 9
(defn recsolve
  [board rows cols idx nums]
  (if (>= idx (count board))
    board
    ;; check whether the current index is a 0
    (if (zero? (get board idx))
      ;; if the cell doesn't have a value yet
      (try-vals board rows cols idx nums 1)
      ;; if we already have a value, skip to next index
      (recsolve board rows cols (inc idx) nums))))

;; A recursive method that plugs in numbers on a sudoku board-
;; When recsolve needs to try numbers on a cell, it calls try-vals
;; which will try fitting each possible number in the cell.
;; Once we have tried every number, we return nil
;; @param board A vector of numbers representing a sudoku
;;   board to be solved
;; @param rows The number of rows in the board
;; @param cols The number of cols in the board
;; @param idx The index for which we are plugging in a value
;; @param nums the number of values that exist on the board
;; - for a normal 3x3 board, this would be 9
(defn try-vals
  [board rows cols idx nums val]
  (if (> val nums)
    nil
    (if (valid-number? board rows cols idx val nums)
      (let [ret (recsolve (assoc board idx val) rows cols (inc idx) nums)]
        (if (nil? ret)
          (try-vals board rows cols idx nums (inc val))
          ret))
      (try-vals board rows cols idx nums (inc val)))))

;; Checks whether a given number would be valid at the 
;; given index. 
;; @param board A vector of numbers representing a sudoku
;;   board to be solved
;; @param rows The number of rows in the board
;; @param cols The number of cols in the board
;; @param idx The index for which we are plugging in a value
;; @param val the number we want to check the validity of
;; @param nums the number of values that exist on the board
(defn valid-number?
  [board rows cols idx val nums]
  (contains? (available-nums board rows cols idx nums) val))

;; Returns a set of all the possible values for the given index
;; @param board A vector of numbers representing a sudoku
;;   board to be solved
;; @param rows The number of rows in the board
;; @param cols The number of cols in the board
;; @param index The index for which we are plugging in a value
;; @param nums the number of values that exist on the board
(defn available-nums
  [board rows cols index nums]
  (into #{} (remove
             (set (keep-indexed
                   #(if (overlap? rows cols index %1 nums)
                      %2
                      nil)
                   board))
             (range 1 (+ nums 1)))))

;; Checks whether a given pos is in the same
;; row, column, or box as the index
;; @param rows The number of rows in the board
;; @param cols The number of cols in the board
;; @param index The index for which we are plugging in a value
;; @param pos The other index for which 
;;   we are checking for an overlap with index
;; @param nums the number of values that exist on the board
(defn overlap?
  [rows cols index pos nums]
  (or (same-col? index pos nums)
      (same-row? index pos nums)
      (same-box? rows cols index pos)))

;; Check whether a and b are the same when modded by divisor
(defn same-col?
  [a b divisor]
  (= (mod a divisor) (mod b divisor)))

;; Check whether a and b are the same when integer divided by a divisor
(defn same-row?
  [a b divisor]
  (= (quot a divisor) (quot b divisor)))

;; Check whether a and b share a sudoku box
(defn same-box?
  [rows cols a b]
  (and (= (box-row-index rows cols a) (box-row-index rows cols b))
       (= (box-col-index rows cols a) (box-col-index rows cols b))))

;; Get the box row index (the vertical 
;; index of the box) for a given position
(defn box-row-index
  [rows cols val]
  (quot val (* rows (* cols cols))))

;; Get the box col index (the horizontal
;; index of the box) for a given position
(defn box-col-index
  [rows cols val]
  (quot (mod val (* cols rows)) rows))
