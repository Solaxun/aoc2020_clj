 (ns aoc2020-clj.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp (io/resource "day16.txt")))
(def parsed (str/split input #"\n\n"))

(def rules
  (->> input
       (re-seq #"(\d+)\-(\d+) or (\d+)\-(\d+)")
       (map rest)
       (map (fn [nums] (map #(Integer/parseInt %) nums)))))

(def field-rules
  (for [r rules :let [[[a b] [c d]] (partition 2 r)
                      valid-range (set (concat (range a (inc b))
                                               (range c (inc d))))]]
    valid-range))

(def my-ticket (map #(Integer/parseInt %) (re-seq #"\d+" (second parsed))))

(def other-tickets
  (->> parsed
       last
       str/split-lines
       rest
       (map #(str/split % #","))
       (map (fn [line] (map #(Integer/parseInt %) line)))))

(def field-rules-combined (reduce set/union field-rules))
;; part 1
(reduce + (for [o (apply concat other-tickets)
                :when (not (contains? field-rules-combined o))]
            o))

;; part 2
(defn valid-ticket? [ticket]
  (every? #(contains? field-rules-combined %) ticket))

(def valid-tickets (filter valid-ticket? other-tickets))

(def options
  (map-indexed (fn [seat-index ticket-section]
                 (vector seat-index
                       (keep-indexed (fn [field-index rules]
                                       (when (empty? (set/difference (set ticket-section) rules))
                                         field-index))
                                     field-rules)))
       (apply map vector valid-tickets)))

;; row-index:rule-index
(def field-orders
  (loop [seen #{}
         field-order []
         [[row-index field-options :as o] & options] (sort-by (comp count second) options)]
    (if-not o
      field-order
      (recur (set/union (set field-options) seen)
             (conj field-order [row-index (first (set/difference (set field-options) seen))])
             options))))

(def departure-indicies
  (set (keep-indexed #(when (str/starts-with? %2 "departure") %1)
                     (str/split-lines (first parsed)))))

(def index-of-departures
  (keep #(when (departure-indicies (second %)) (first %)) field-orders))

(apply * (map #(nth my-ticket %) index-of-departures))