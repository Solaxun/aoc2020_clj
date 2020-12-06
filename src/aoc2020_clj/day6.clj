(ns aoc2020-clj.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp (io/resource "day6.txt")))
(def parsed (str/split input #"\n\n"))

(defn count-questions [questions]
  (-> questions
      (str/replace #"\n" "")
      set
      count))

;; part 1
(reduce + (map count-questions parsed))

;; part 2
(->> parsed
     (map #(str/split % #"\n"))
     (map (partial map set))
     (map (partial apply set/intersection))
     (map count)
     (reduce +))
