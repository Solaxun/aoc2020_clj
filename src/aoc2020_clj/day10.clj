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

;; additional attempt, this one works but was overly complicated and replaced by the above
(defn count-paths [adapters]
  (let [cache (atom {})]
    (letfn [(sum-paths [a]
              (if-let [cached (@cache a)]
                cached
                (if-let [choices (seq (filter #(adapter-fits? a %) adapters))]
                  (let [paths-from-here (reduce + (map sum-paths choices))]
                    (swap! cache assoc a paths-from-here)
                    paths-from-here)
                  1)))]
      (sum-paths 0))))
(count-paths (arrange-adapters parsed))

;; this one didn't work because it tried to generate all possible paths, which for the input has an upper
;; bound of 3 ^ input-size (101), or a number to big to even reasonably print, let alone trillions
(defn dfs [adapters]
  (loop [paths [[(first adapters)]]
         res 0
         i 0]
    (if (empty? paths)
      res
      (let [path (peek paths)
            choices (filter #(adapter-fits? (last path) %) adapters)
            new-paths (map #(conj path %) choices)]
        #_(println path choices new-paths)
        (if (empty? new-paths)
          (recur (pop paths) (inc res) (inc i))
          (recur (into (pop paths) new-paths) res (inc i)))))))
