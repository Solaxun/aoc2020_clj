(ns aoc2020-clj.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [aoc2020-clj.utils.search :as search]
            [clojure.math.combinatorics :as combs]))

(def input (slurp (io/resource "day10.txt")))
(def parsed (mapv read-string (str/split-lines input)))

(defn arrange-adapters [adapters]
  (sort (conj adapters 0 (+ 3 (apply max adapters)))))

;; part 1
(->> parsed
     arrange-adapters
     (partition 2 1)
     (map (fn [[a b]] (- b a)))
     frequencies
     (#(select-keys % [1 3]))
     vals
     (reduce *))

;; part 2
(defn adapter-fits? [current-adapter other-adapter]
  (< current-adapter other-adapter (+ 4 current-adapter)))

(defn possible-arrangements [root adapters]
  (if-let [choices (seq (filter #(adapter-fits? root %) adapters))]
    (reduce + (map #(possible-arrangements % adapters) choices))
    1))

(def possible-arrangements (memoize possible-arrangements))
(possible-arrangements 0 parsed)