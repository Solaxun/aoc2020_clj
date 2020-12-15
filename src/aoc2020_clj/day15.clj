(ns aoc2020-clj.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
;; input small enough to be typed, but leaving this for consistency
(def input (slurp (io/resource "day15.txt")))
(def parsed (map #(Integer/parseInt %) (str/split input #",")))

(defn stupid-elves [start-nums target]
  (loop [seen (into {} (map-indexed #(vector %2 (inc %1)) (drop-last start-nums)))
         last-spoken (last start-nums)
         i (count start-nums)]
    (if (= i target)
      last-spoken
      (recur (assoc seen last-spoken i)
             (if (seen last-spoken) (- i (seen last-spoken)) 0)
             (inc i)))))
;; part 1
(stupid-elves parsed 2020)
;; part 2 ~ 40 seconds, too lazy to optimize
(stupid-elves parsed 30000000)