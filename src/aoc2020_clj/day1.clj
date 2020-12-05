(ns aoc2020-clj.day1
  (:require [clojure.java.io :as io]
            [aoc2020-clj.utils.search :as search]))

(def input (slurp (io/resource "day1.txt")))

(defn part-1 [input]
  (->> input
       (re-seq #"\d+")
       (map read-string)))

;; part 1
(for [x (part-1 input)
      y (part-1 input)
      :when (= (+ x y) 2020)]
  (* x y))

;; part 2
(for [x (part-1 input)
      y (part-1 input)
      z (part-1 input)
      :when (= (+ x y z) 2020)]
  (* x y z))

;; lessons learned:

;; I took the obvious approach, which I knew would be slow
;; but figured it wouldn't matter based on input size.  The better way to do this
;; is to create a set of all the numbers, loop through each number, and any time
;; that 2020 - number is in the set, you know that those two numbers will sum to
;; 2020.  This is 0^n for the search, plus the one-time cost of initalizing the
;; set.
;;
;; See Chouser solution for an implementation of this.
;;
;; Part 2 is the same idea, but you are forced to do a n^2 loop and then from
;; there, check if the number at the inner loop can be sutracted from 2020 and
;; found in the set.  Same idea, one more loop iteration.
