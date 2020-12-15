(ns aoc2020-clj.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combs]))

(def input (slurp (io/resource "day14.txt")))

(defn parse-line [line]
  (if (str/starts-with? line "mask")
    [:mask nil (last (str/split line #" = "))]
    [:mem
     (->> line (re-find #"\[(\d+)\]") last (Integer/parseInt))
     (-> line (str/split #" = ") last (Integer/parseInt))]))

(def parsed (map parse-line (str/split-lines input)))

(defn left-pad-str [total-size pad xs]
  (str (str/join (repeat (- total-size (count xs)) pad))
       xs))

(defn int->bin36 [n]
  (left-pad-str 36 "0" (Integer/toBinaryString n)))

(defn mask [mask num]
  (str/join (map (fn [m n] (if (= \X m) n m)) mask (int->bin36 num))))

(defn replace-matches-with [cs target replacements]
  (first (reduce (fn [[new-cs replacements] c]
                   (if (= target c)
                     [(str new-cs (first replacements)) (rest replacements)]
                     [(str new-cs c) replacements]))
                 ["" replacements]
                 cs)))

(defn permute-bits [bits]
  (let [xcount (->> bits (filter (partial = \X)) count)]
    (for [rs (combs/selections "10" xcount)]
      (replace-matches-with bits \X rs))))

(defn mask2 [mask num]
  (str/join (map (fn [m n] (if (or (= \1 m) (= \X m)) m n)) mask (int->bin36 num))))

(defn solve [& {part :part}]
  (->> (reduce (fn [{:keys [cur-mask registers] :as state} [op reg num]]
                 (if (= op :mask)
                   (assoc state :cur-mask num)
                   (if (= part 1)
                     (update state :registers assoc reg (Long/parseLong (mask cur-mask num) 2))
                     (update state :registers (partial apply merge)
                             (reduce #(assoc %1 %2 num)
                                     registers
                                     (map #(Long/parseLong % 2) (permute-bits (mask2 cur-mask reg))))))))
               {:cur-mask nil :registers {}}
               parsed)
       :registers
       vals
       (reduce +)))
(solve :part 1)
(solve :part 2)
