(ns aoc2020-clj.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day13.txt")))
(def parsed (str/split-lines input))


(def target 939)
;; find each multiple larger than 939 then take smallest of those minus 939
(def foo [7, 13, 59, 31, 19])

(defn factors-less-than [n threshold]
  (last (take-while #(< % threshold)
                    (map #(* n %) (range)))))
(defn factor? [n] (every? #(zero? (mod n %)) [1 5 6 8 3]))
(map #(factors-less-than % (+ 939 %)) foo)
(filter #(zero? (mod (+ 2 %) 3)) (take 100 (iterate (partial + 5) 5)))
(let [[target buses] parsed
      t (Long/parseLong target)
      [_ & bs] (map #(Integer/parseInt %) (re-seq #"\d+" buses))]
  (map #(factors-less-than % (+ t %)) bs))

;; id * waiting (944 - 939)
(- 1007130 1007125)
(* 5 569)

(defn build-constraints [nums]
  (keep (fn [[i num]] (when (not= num "x") [i (Integer/parseInt num)]))
        (map vector (range) (str/split nums #","))))

;; every busid must evendly divide the timestamp plus delta for the busid (how many min can pass)
(defn test-timestamp [ts constraint-vec]
  (every? (fn [[delta busid]]
            (zero? (mod (+' ts delta)
                      busid)))
          constraint-vec))
(test-timestamp 3166399417896 (build-constraints (last parsed)))
(map (partial apply +) (build-constraints (last parsed)))
;; (test-timestamp 1068781 (build-constraints "7,13,x,x,59,x,31,19"))
(let [constraints  (build-constraints "5,8,3")
      [_ first-bus] (first constraints)]
  (first (filter #(test-timestamp % constraints)
                 (iterate (partial +' first-bus) first-bus))))

(let [constraints  (build-constraints (last parsed))
      [_ first-bus] (first constraints)]
  (first (filter #(test-timestamp % constraints)
                 (iterate (partial +' first-bus) 100000000000000))))

;; deltas acceptable between each number, e.g. first num has no delta, second num is 3 + first num
(defn multiples-of [n]
  (rest (iterate (partial +' n) n)))
(build-constraints (last parsed))

(def input1 "17,x,13,19")
(def input2 "67,7,59,61")
(def input3 "67,x,7,59,61")
(def input4 "67,7,x,59,61")
(def input5 "1789,37,47,1889")

(first (filter (fn [m] (zero? (mod (+ m 3) 19)))
               (filter (fn [m] (zero? (mod (+ m 2) 13)))
                       (multiples-of 17))))

(first (reduce (fn [multiples [offset busid]]
                 (filter (fn [m] (zero? (mod (+ m offset) busid)))
                         multiples))
               (multiples-of (second (first (build-constraints (last parsed)))))
               (rest (build-constraints (last parsed)))))

(map second (build-constraints (last parsed)))
(map #(mod 2265213528143033 %)
      (map second (build-constraints (last parsed))))