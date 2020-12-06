(ns aoc2020-clj.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day5.txt")))
(def parsed (str/split-lines input))

(defn binary-split [[low high] seat]
  (case seat
    (\L \F) [low (-> (+ low high) (/ 2))]
     [(/ (+ high low) 2) high]))

(defn get-seat [seating bounds]
  (let [[low high] (reduce binary-split bounds seating)]
    (if (= \F (last seating))
      low
      (dec high))))

(defn solve [seating]
  (let [[row col] (split-at 7 seating)]
    (+ (* 8 (get-seat row [0 128]))
       (get-seat col [0 8]))))

; part 1
(->> parsed (map solve) (apply max))
;; part 2
(->> parsed
     (map solve)
     sort
     (partition 2 1)
     (filter (fn [[x y]] (not= x (dec y)))))

;; lessons learned:

;; I missed the obvious (in hindsight) trick - you could just convert to a binary
;; string where fronts are 0 and backs are 1, then simply get the integer represented
;; by that binary string.  See yet again transducer's github for an example of this
