(ns aoc2020-clj.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp (io/resource "day9.txt")))
(def parsed (mapv read-string (str/split-lines input)))

(defn sums-to? [target nums]
  (first (for [x nums y nums :when (and (not= x y) (= target (+ x y)))]
           target)))

(defn no-sum-25 [nums]
  (loop [preamble 25]
    (let [prev25 (subvec nums (- preamble 25) preamble)
          target (nums preamble)]
      (if (sums-to? target prev25)
        (recur (inc preamble))
        target))))

;; part 1
(def part1-answer (no-sum-25 parsed))

;; find a contiguous block that sums to part1 answer - we know that the number
;; cannot be larger than part1 answer, so filter those values out first
(defn contiguous-sum? [chunk-size]
  (->> parsed
       (filter (partial > part1-answer))
       (partition chunk-size 1)
       (filter (fn [chunk] (= part1-answer (apply + chunk))))
       first))

;; part 2
(->> (range 2 (count parsed))
     (keep contiguous-sum?)
     first
     ((juxt (partial apply min) (partial apply max)))
     (apply +))