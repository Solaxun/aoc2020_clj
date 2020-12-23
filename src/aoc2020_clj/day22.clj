(ns aoc2020-clj.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.set :as set]))

(def input (slurp (io/resource "day22.txt")))

(defn get-ints [line]
  (map #(Integer/parseInt %) (re-seq #"\d+" line)))

 (def decks
  (let [[p1 p2] (map get-ints (str/split input #"\n\n"))]
    {:player1 (vec (rest p1)) :player2 (vec (rest p2))}))

(defn play-round [{[p1 & p1s] :player1 [p2 & p2s] :player2}]
  (if (> p1 p2)
    {:player1 (concat p1s [p1 p2]) :player2 p2s}
    {:player1 p1s :player2 (concat p2s [p2 p1])}))

(->> decks
     (iterate play-round)
     (filter (fn [{:keys [player1 player2]}] (or (empty? player1) (empty? player2))))
     first
     ((fn [{:keys [player1 player2]}] (or (seq player1) (seq player2))))
     reverse
     (map-indexed (fn [i x] (* (inc i) x)))
     (reduce +))

(defn play-round-recursive [game]
  (loop [seen #{}
         {[p1 & p1s :as player1] :player1 [p2 & p2s :as player2] :player2 :as game} game]
    (cond (seen game) [:player1 player1]
          (empty? player1) [:player2 player2]
          (empty? player2) [:player1 player1]
          :else (let [winner (if (and (<= p1 (count p1s)) (<= p2 (count p2s)))
                               (play-round-recursive {:player1 (take p1 p1s)
                                                      :player2 (take p2 p2s)})
                               (if (> p1 p2) [:player1 player1]
                                             [:player2 player2]))]

                  (recur (conj seen game)
                         (if (= (first winner) :player1)
                           {:player1 (concat p1s [p1 p2]) :player2 p2s}
                           {:player1 p1s :player2 (concat p2s [p2 p1])}))))))

(->> (play-round-recursive decks)
     second
     reverse
     (map-indexed (fn [i x] (* (inc i) x)))
     (reduce +))