(ns aoc2020-clj.day17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combs]))

(def input (slurp (io/resource "day17.txt")))
(def parsed (str/split-lines input))

(defn neighbors-ndims [ndims coord]
  (for [deltas (combs/selections [-1 0 1] ndims)
        :when (apply not= 0 deltas)]
    (mapv + coord deltas)))

(defn initial-state [ndims]
  (let [gen1-given (into {} (for [[x row]  (map-indexed vector parsed)
                                  [y cube] (map-indexed vector row)]
                              [(into [x y] (repeat (- ndims 2) 0)) cube]))
        gen1-remaining (zipmap (combs/selections [-1 0 1] ndims) (repeat \.))]
    (merge gen1-remaining gen1-given)))

(defn cube-on? [ndims coord cube gen]
  (let [oncount (count (keep (fn [coord] (when (= \# (get gen coord)) coord))
                             (neighbors-ndims ndims coord)))]
    (contains? #{[2 \#] [3 \#] [3 \.]}
               [oncount cube])))

(defn evolve [ndims gen]
  (->> gen
       (mapcat (fn [[coord _]] (neighbors-ndims ndims coord)))
       (#(zipmap % (repeat \.)))
       (#(merge % gen))
       (keep (fn [[coord cube]] (when (cube-on? ndims coord cube gen)
                                  [coord \#])))
       (into {})))

(defn darwin [ndims starting-gen]
  (->> ndims
       starting-gen
       (iterate (partial evolve ndims))
       (#(nth % 6))
       (filter (comp (partial = \#) val))
       count))

(darwin 3 initial-state)
(darwin 4 initial-state)