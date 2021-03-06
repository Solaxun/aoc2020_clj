(ns aoc2020-clj.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp (io/resource "day8.txt")))
(def parsed (str/split-lines input))

(defn execute [instr]
  (loop [i 0
         acc 0
         seen #{}]
    (cond (seen i) {:looped true :acc acc}
          (>= i (count instr)) {:looped false :acc acc}
      :else (let [[op n] (str/split (instr i) #" ")
                  n (read-string n)]
              (case op
                "jmp" (recur (+ i n) acc (conj seen i))
                "acc" (recur (inc i) (+ acc n) (conj seen i))
                "nop" (recur (inc i) acc (conj seen i)))))))
;; part 1
(execute parsed)
;; part 2
(defn get-locs [op-type input]
  (keep-indexed (fn [i instr]
                  (when (-> instr (str/split #" ") first (= op-type))
                    i))
                input))

(defn replace-instr [instr old new]
  (map (fn [loc]
         (assoc instr loc (str new " " (-> loc instr (str/split #" ") second))))
       (get-locs old instr)))

;; run every possible replacement and keep the one that terminates
(filter (fn [{:keys [looped acc]}]
          (not looped))
        (map execute
             (concat (replace-instr parsed "jmp" "nop")
                     (replace-instr parsed "nop" "jmp"))))

;; lessons learned:

;; I generated every possible replacement using `replace-instr`, and only then tested
;; the program to see if it would terminate or not.  A better approach taken by most
;; is to start executing the program, and at each step in the execution if the instr
;; is jmp or nop, replace with the other and return if it terminates.  This way you
;; return the first time you encounter an instruction that would result in termination,
;; avoiding the need to generate the remaining replacements.