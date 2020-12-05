(ns aoc2020-clj.day2
  (:require [clojure.java.io :as io]))

(def input (slurp (io/resource "day2.txt")))

(def parsed
  (re-seq #"(\d+)-(\d+) (\w): (\w+)" input))

(defn valid-pw? [[_ mini maxi target pw]]
  (let [cnt (frequencies pw)
        target (first target)]                              ; convert string to char
    (and (>= (cnt target 0) (read-string mini))
         (<= (cnt target 0) (read-string maxi)))))

(defn valid-pw-2? [[_ mini maxi target pw]]
  (let [target (first target)
        a      (get pw (dec (read-string mini)))
        b      (get pw (dec (read-string maxi)))]
    (and
      (or (= a target) (= b target))
      (not (and ( = b target) (= a target))))))

;; part 1
(count (filter valid-pw? parsed))
;; part 2
(count (filter valid-pw-2? parsed))

;; lessons learned:

;; I spent more time than I should have trying to figure out how to convert a
;; single character string to a char.  Since a string is always a seq of chars,
;; a single character string is no different.  Simply calling first will give you
;; the first char of the seq.  A clean way to do this is via destructuring which

;; e.g. (let [[c] "s"] c) => \s

;; Also I spread the parsing over too much of the program, should do it all in
;; one function and then call the various valid? predicates with already parsed
;; data.

;; See again Erwin Rooijakker's github (handle transducer) for a nice version.