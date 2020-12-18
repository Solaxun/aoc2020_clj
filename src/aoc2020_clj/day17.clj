(ns aoc2020-clj.day17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.set :as set]))

(def input (slurp (io/resource "day17.txt")))
(def parsed (str/split-lines input))

(defn neighbors-ndims [ndims coord]
  (for [deltas (combs/selections [1 0 -1] ndims)
        :when (apply not= 0 deltas)]
    (mapv + coord deltas)))

(def initial-state
  (into {} (for [[x row]  (map-indexed vector parsed)
                 [y cube] (map-indexed vector row)
                 z [1 0 -1]
                 :let [cube (if (zero? z)
                              [[x y z] cube]
                              [[x y z] \.])]]
             cube)))

(def initial-state2
  (into {} (for [[x row]  (map-indexed vector parsed)
                 [y cube] (map-indexed vector row)
                 z [1 0 -1]
                 w [1 0 -1]
                 :let [cube (if (= 0 z w)
                              [[x y z w] cube]
                              [[x y z w] \.])]]
             cube)))

(defn cube-on? [ndims coord cube gen]
  (let [oncount (count (keep (fn [coord] (when (= \# (get gen coord)) coord))
                             (neighbors-ndims ndims coord)))]
    (contains? #{[2 \#] [3 \#] [3 \.]}
               [oncount cube])))

(defn evolve [ndims gen]
  (->> gen
       (mapcat (fn [[coord cube]] (neighbors-ndims ndims coord)))
       (#(zipmap % (repeat \.)))
       (#(merge % gen))
       (keep (fn [[coord cube]] (when (cube-on? ndims coord cube gen)
                                  [coord \#])))
       (into {})))

(count (filter (comp #(= % \#) val) (nth (iterate (partial evolve 3) initial-state) 6)))
(count (filter (comp #(= % \#) val) (nth (iterate (partial evolve 4) initial-state2) 6)))