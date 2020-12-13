(ns aoc2020-clj.utils.grids)

(defn neighbors8 [[x y]]
  (for [i [-1 0 1] j [-1 0 1]
        :when (not= i j 0)]
    [(+ x i) (+ j y)]))

(defn neighbors4 [[x y]]
  (for [i [-1 0 1] j [-1 0 1]
        :when (and (not= (Math/abs i) (Math/abs j))
                   (not= i j 0))]
    [(+ x i) (+ j y)]))

(defn manhattan-distance [pt1 pt2]
  (reduce + (map (fn [a b] (Math/abs (- a b))) pt1 pt2)))