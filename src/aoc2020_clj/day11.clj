(ns aoc2020-clj.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [aoc2020-clj.utils.search :as search]
            [aoc2020-clj.utils.grids :as grids]
            [clojure.math.combinatorics :as combs]))

(def input (slurp (io/resource "day11.txt")))
(def parsed (str/split-lines input))

(defmacro forv [& body]
  `(vec (for ~@body)))

(defn generation [grid neighbor-func threshold]
  (forv [[i row] (map-indexed vector grid)]
        (forv [[j seat] (map-indexed vector row)]
          (let [neighbors (frequencies (neighbor-func [i j] grid))]
                          (case seat
                            \L (if-not (neighbors \#) \# seat)
                            \# (if (>= (neighbors \# 0) threshold) \L seat)
                            seat)))))

(defn in-bounds? [[x y] grid]
  (and (contains? grid x)
       (contains? (grid x) y)))

(defn line-of-sight-neighbors [[xloc yloc] grid]
  (map (fn [movedir]
         (let [view-path (take-while #(in-bounds? % grid)
                                     (rest (iterate #(mapv + % movedir) [xloc yloc])))]
           (some #{\L \#} (map #(get-in grid %) view-path))))
       (grids/neighbors8 [0 0])))

(defn immediate-neighbors [[x y] board]
  (map #(get-in board %) (grids/neighbors8 [x y])))

(defn evolve [first-gen neighbor-func threshold]
  (reduce (fn [prev-gen cur-gen]
            (if (= prev-gen cur-gen)
              (reduced cur-gen)
              cur-gen))
          first-gen
          (rest (iterate #(generation % neighbor-func threshold) first-gen))))

(defn count-occupied [generation]
  (-> generation
      (->> (apply concat))
      (frequencies)
      (get \#)))

(count-occupied (evolve parsed immediate-neighbors 4))
(count-occupied (evolve parsed line-of-sight-neighbors 5))
