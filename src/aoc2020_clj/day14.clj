(ns aoc2020-clj.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combs]))

(def input (slurp (io/resource "day14.txt")))
(def parsed (str/split-lines input))

(defn left-pad-str [total-size pad xs]
  (str (str/join (repeat (- total-size (count xs)) pad))
       xs))

(defn int->bin36 [n]
  (left-pad-str 36 "0" (Integer/toBinaryString n)))

(defn mask [mask num]
  (str/join (map (fn [m n] (if (= \X m) n m)) mask (int->bin36 num))))

(defn get-reg [instr]
  (Long/parseLong (last (re-find #"\[(\d+)\]" instr))))

(defn get-num [instr]
  (Long/parseLong (last (re-find #" = (\d+)" instr))))
;; part 1
(->> (reduce (fn [{:keys [cur-mask registers] :as state} instr]
               (if (str/starts-with? instr "mask")
                 (assoc state :cur-mask (last (str/split instr #"mask = ")))
                 (assoc-in state [:registers (get-reg instr)]
                           (Long/parseLong (mask cur-mask (get-num instr)) 2))))
             {:cur-mask nil :registers {}}
             parsed)
     :registers
     vals
     (reduce +))
;; part 2
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

(->> (reduce (fn [{:keys [cur-mask registers] :as state} instr]
               (if (str/starts-with? instr "mask")
                 (assoc state :cur-mask (last (str/split instr #"mask = ")))
                 (let [r (get-reg instr)
                       n (get-num instr)
                       ps (map #(Long/parseLong % 2) (permute-bits (mask2 cur-mask r)))]
                   (update state :registers (partial apply merge) (reduce #(assoc %1 %2 n) registers ps)))))
             {:cur-mask nil :registers {}}
             parsed)
     :registers
     vals
     (reduce +))
