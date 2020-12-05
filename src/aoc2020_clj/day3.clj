(ns aoc2020-clj.day3
  (:require [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "day3.txt"))))

(defn count-trees [x-slope y-slope]
  (loop [input input
         x 0
         trees 0]
    (if-not (seq input)
      trees
      (recur (drop y-slope input)
             (+ x x-slope)
             (if (= \# (-> input first cycle (nth x)))
               (inc trees)
               trees)))))
;; part 1
(count-trees 3 1)
;; part 2
(reduce * (map (fn [[x y]] (count-trees x y)) [[1 1] [3 1] [5 1] [7 1] [1 2]]))

;; lessons learned:

;; this isn't a bad solution, and the cycle idea appears in most functional
;; solutions, whereas modulo tends to be there in the iterative approach.

;; Another solution (see transducer's github [Erwin Rooijakker]) is to first
;; come up with all coordinates using iterate, then from there index in to
;; the board by getting the row, and then calling cycle on the result before
;; getting the position w/in the row (same as my approach once you have the
;; row)