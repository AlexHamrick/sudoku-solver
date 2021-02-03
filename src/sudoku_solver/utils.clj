(ns sudoku-solver.utils)
(require '[sudoku-solver.solver :as solver])
(require '[clojure.core.matrix :as matrix])
(require '[clojure.string :as str])

(declare replace-random
         rand-first-line)

;; Creates a "random" rows x cols sudoku board-
;; The current implementation makes a randomly permuted top row, 
;; then fills in some random indices, solves the board, and removes 
;; 75% of the values... 
;; TODO try the diagonal permuted box generator
(defn generate
  [rows cols]
  (let
   [blank-pct 0.75
    rand-pct 0.08
    nums (* rows cols)
    l (rand-first-line nums)
    ;; Replace some random indices with random values
    list (replace-random 
          l #(inc (rand-int nums)) (* rand-pct (count l)) rows cols)
    result (solver/recsolve (vec (flatten list)) rows cols 0 nums)
    blank-ct (* blank-pct (count result))]
    (if result
      (matrix/reshape
       (replace-random result (constantly 0) blank-ct rows cols) [nums nums])
      (generate rows cols))))

;; Replaces some number (num-to-replace) of random indices in the board
;; with a value defined by the val-fn
(defn replace-random
  [list val-fn num-to-replace rows cols]
  (loop [board list, remaining num-to-replace]
    (let [r (val-fn)
          idx (rand-int (count board))]
      (if (<= remaining 0)
        board
        (if (or (= r 0)
                (solver/valid-number? board rows cols idx r (* rows cols)))
          (recur (assoc board idx r) (dec remaining))
          (recur board (dec remaining)))))))

;; A helper method for generating a board with a randomly permuted
;; first line
(defn rand-first-line
  [nums]
  (let [first (shuffle (range 1 (inc nums)))
        zeros (repeat (- (* nums nums) nums) 0)]
    (vec (flatten (conj first zeros)))))

;; Reads in a file as a vector (ready to be solved)
(defn read-file
  [filename]
  (map #(. Integer parseInt %)
       (str/split (slurp filename) #"\s+")))

;; Directly solves the board in the given file
(defn solve-file
  [filename rows cols]
  (solver/solve (read-file filename) rows cols))
