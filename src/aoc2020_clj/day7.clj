(ns aoc2020-clj.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp (io/resource "day7.txt")))
(def parsed (str/split-lines input))

(defn parse-line [line]
  (let [[outer inner] (str/split line #" bags contain ")
        inner (str/split inner #", ")]
    {outer  (mapv (comp vec reverse rest #(re-find #"(\d+) (.*) bags?" %))
                  inner)}))

(def leaf-nodes
  (->> parsed
       (filter #(str/ends-with? % "contain no other bags."))
       (map parse-line)))

(def not-leaf-nodes
  (->> parsed
       (remove #(str/ends-with? % "contain no other bags."))
       (map parse-line)))

(def graph (apply merge (concat leaf-nodes not-leaf-nodes)))

(defn find-parents [g target]
  (keys (filter (fn [[bag children]]
                  (some (fn [[b c]] (= b target))
                        children))
                g)))

(defn part1 [target]
  (mapcat #(concat [%] (part1 %)) (find-parents graph target)))

;; part 1

;; find direct parents where children contain shiny gold
;; then for each of those find their parents, etc.
(count (set (part1 "shiny gold")))

;; part 2

;; for every bag, add to the number of bags the sum of num bags times
;; num bags children contain.
(defn count-bags [g target]
  (reduce (fn [total [bag n-bags]]
            (let [bags (read-string (or n-bags "0"))]
              (+ total (+ bags (* bags (count-bags g bag))))))
          0
          (g target)))

(count-bags graph "shiny gold")