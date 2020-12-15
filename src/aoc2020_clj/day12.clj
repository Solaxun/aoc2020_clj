(ns aoc2020-clj.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2020-clj.utils.grids :as grids]))

(def input (slurp (io/resource "day12.txt")))
(def parsed (str/split-lines input))

(def directions
  (map (fn [dir]
         (let [[_ dir num] (re-matches #"([NESWLRF])(\d+)" dir)]
           [dir (Integer/parseInt num)]))
       parsed))

(def ship       {:heading "E" :coord [0 0]})
(def waypoint   [-1 10])
(def moves      {"N" [-1 0] "S" [1 0] "E" [0 1] "W" [0 -1]})
(def compass    ["N" "E" "S" "W"])

(defn turn-n-degrees [current-heading rotation-dir degrees]
  (let [i (.indexOf compass current-heading)
        steps  (/ degrees 90)]
    (if (= rotation-dir "L")
      (compass (mod (- i steps) (count compass)))
      (compass (mod (+ i steps) (count compass))))))

(defn rotate-around-origin [[wp-x wp-y] rotation-dir degrees]
  (let [clockwise-rotations {90  (fn [x y] [y (- x)])
                             180 (fn [x y] [(- x) (- y)])
                             270 (fn [x y] [(- y) x])}]
    (case rotation-dir
      "R" ((clockwise-rotations        degrees)  wp-x wp-y)
      "L" ((clockwise-rotations (- 360 degrees)) wp-x wp-y))))

(defn move-forward [coord move-vec magnitude]
  (let [move (mapv (partial * magnitude) move-vec)]
    (mapv + coord move)))

(defn travel [fleet directions]
  (reduce (fn [{waypoint :waypoint {:keys [coord heading]} :ship :as fleet}
               [dir num]]
            (case dir
              ("R" "L") (if waypoint
                          (update fleet :waypoint rotate-around-origin dir num)
                          (update-in fleet [:ship :heading] turn-n-degrees dir num))
              "F" (update-in fleet [:ship :coord] move-forward (or waypoint (moves heading)) num)
              ("N" "S" "E" "W") (if waypoint
                                  (update fleet :waypoint move-forward (moves dir) num)
                                  (update-in fleet [:ship :coord] move-forward (moves dir) num))))
          fleet
          directions))

(grids/manhattan-distance [0 0] (get-in (travel {:ship ship :waypoint nil} directions) [:ship :coord]))
(grids/manhattan-distance [0 0] (get-in (travel {:ship ship :waypoint waypoint} directions) [:ship :coord]))