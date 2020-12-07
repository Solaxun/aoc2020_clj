(ns aoc2020-clj.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp (io/resource "day6.txt")))
(def parsed (str/split input #"\n\n"))

(defn count-questions [questions]
  (-> questions
      (str/replace #"\n" "")
      set
      count))

;; part 1
(reduce + (map count-questions parsed))

;; part 2
(->> parsed
     (map str/split-lines)
     (map (partial map set))
     (map (partial apply set/intersection))
     (map count)
     (reduce +))

(def repath "^ENWWW(NEEE|SSE(EE|N))$")
(def repath "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")
(def repath "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
;; lisp cheating :P
(def parsed
  (read-string (str "("
                    (subs repath 1 (dec (count repath)))
                    ")")))

(defn parse-directions [segment]
  (println "parse-dir" (str/split segment #"\|"))
  (let [res (str/split segment #"\|")]
    (if (str/ends-with? segment "|")
      (conj res "")
      res)))

(defn gen-path [acc pat]
  (cond (not (some? pat)) [acc]
        (symbol? pat) (parse-directions (str pat))
        (list? pat) (mapcat #(gen-path (str acc %) (next pat))
                             (gen-path "" (first pat)))))

;; now get coord that you end up with for each sequence, and count steps to get there
(defn follow-dirs [dirs]
  (reduce (fn [acc cur] (mapv + acc cur)) [0 0]
          (map {\E [0 1] \N [-1 0] \S [1 0] \W [0 -1]}
               dirs)))

(reduce (fn [pos->steps dirs]
          (let [pos (follow-dirs dirs)
                moves (count dirs)]
            (update pos->steps pos (fnil conj []) moves)))
        {}
        (gen-path "" parsed))

(count (apply max-key count (gen-path "" parsed)))