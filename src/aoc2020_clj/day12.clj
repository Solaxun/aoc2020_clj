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

(defn move-forward [coord heading steps]
  (reduce #(mapv + %1 %2)
    coord
    (repeat steps (moves heading))))

(defn move-toward-waypoint [coord waypoint waypoint-magnitude]
  (let [move (mapv (partial * waypoint-magnitude) waypoint)]
    (mapv + coord move)))

(defn travel [ship directions]
  (reduce (fn [{:keys [heading coord] :as ship} [dir num]]
            (case dir
              ("R" "L") (update ship :heading turn-n-degrees dir num)
              "F" (update ship :coord move-forward heading num)
              ("N" "S" "E" "W") (update ship :coord move-forward dir num)))
    ship
    directions))
;; part 1
(grids/manhattan-distance [0 0] (:coord (travel ship directions)))
;; part 2
(def fleet {:ship ship :waypoint waypoint})

(defn travel2 [fleet directions]
  (reduce (fn [{waypoint :waypoint {:keys [coord heading]} :ship :as fleet}
               [dir num]]
            (case dir
              ("R" "L") (update fleet :waypoint rotate-around-origin dir num)
              "F" (update-in fleet [:ship :coord] move-toward-waypoint waypoint num)
              ("N" "S" "E" "W") (update fleet :waypoint move-forward dir num)))
          fleet
          directions))

(grids/manhattan-distance [0 0] (get-in (travel2 fleet directions) [:ship :coord]))