(ns aoc2020-clj.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp (io/resource "day16.txt")))
(def parsed (str/split input #"\n\n"))

(def field-rules
  (->> input
       (re-seq #"(\d+)\-(\d+) or (\d+)\-(\d+)")
       (map rest)
       (map (fn [rs] (map #(Integer/parseInt %) rs)))
       (map (fn [[a b c d]] (concat (range a (inc b)) (range c (inc d)))))))

(def my-ticket
  (map #(Integer/parseInt %) (re-seq #"\d+" (second parsed))))

(def other-tickets
  (->> parsed
       last
       str/split-lines
       rest
       (map #(str/split % #","))
       (map (fn [line] (map #(Integer/parseInt %) line)))))

(def field-rules-combined
  (reduce set/union (map set field-rules)))

;; part 1
(reduce + (for [o (apply concat other-tickets)
                :when (not (contains? field-rules-combined o))]
            o))

;; part 2
(defn valid-ticket? [ticket]
  (every? #(contains? field-rules-combined %) ticket))

(def rowix->possible-fields
  (map-indexed (fn [seat-index ticket-section]
                 (vector seat-index
                         (set (keep-indexed (fn [field-index rules]
                                              (when (empty? (set/difference (set ticket-section) rules))
                                                field-index))
                                            (map set field-rules)))))
       (apply map vector (filter valid-ticket? other-tickets))))

(def field-orders
  (second (reduce (fn [[seen fieldix->rowix] [rowix possible-field-indices]]
                    [(set/union possible-field-indices seen)
                     (assoc fieldix->rowix (first (set/difference possible-field-indices seen))
                                           rowix)])
                  [#{} {}]
                  (sort-by (comp count second) rowix->possible-fields))))

(def departure-indices
  (set (keep-indexed #(when (str/starts-with? %2 "departure") %1)
                     (str/split-lines (first parsed)))))

(def departure-row-indices
  (map #(field-orders %) departure-indices))

(apply * (map #(nth my-ticket %) departure-row-indices))