(ns aoc2020-clj.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn height-validator [hgt unit]
  (let [hgt (-> hgt (Long/parseLong))]
    (if (= unit "cm")
      (<= 150 hgt 193)
      (<= 59  hgt 76))))

(def passport-rules
  [{:pat #"byr:(\d{4})"       :validator (fn [yr] (-> yr read-string (#(<= 1920 % 2002))))}
   {:pat #"iyr:(\d{4})"       :validator (fn [yr] (-> yr read-string (#(<= 2010 % 2020))))}
   {:pat #"eyr:(\d{4})"       :validator (fn [yr] (-> yr read-string (#(<= 2020 % 2030))))}
   {:pat #"hgt:(\d+)(cm|in)"  :validator height-validator}
   {:pat #"hcl:#([0-9a-f]+)"  :validator #(= (count %) 6)}
   {:pat #"ecl:([a-z]{3})"    :validator (fn [pat] (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} pat))}
   {:pat #"pid:(\d+)"         :validator #(= (count %) 9)}])

(def input (slurp (io/resource "day4.txt")))

(defn all-fields-present? [passport]
  (every? #(re-find (re-pattern %) passport) required-fields))

(defn validate [validator]
  (->> (str/split input #"\n\n")
       (filter validator)
       count))
;; part 1
(validate all-fields-present?)
;; part 2
(defn validate-rule [{:keys [pat validator]} passport]
  (when-let [[whole-match & match-groups] (re-find pat passport)]
    (apply validator match-groups)))

(defn all-rules-followed? [pw]
  (every? #(validate-rule % pw) passport-rules))

(validate all-rules-followed?)

;; day 4 learnings:

;; oh boy... wasted hours before looking for a hint.  I seemed to have forgotten
;; that \d{9} matches 9 digits *anywhere* in the string, so it will match 9+ digits
;; not exactly 9. In the above, I originally had `pid:\d{9}`, and since I'm searching
;; anywhere in the string for that pattern, pid:123456789 would match, but so would
;; pid:0123456789 which would be invalid since it's 10 digits.  Matching *exactly*
;; `n` digits required either bounding it with ^pat$ which only works if the whole
;; string matches, e.g. starts with and ends with pat, so I chose to capture everything
;; and verify the count in the validator.  Aside from this one bug I'm relatively happy
;; with the data driven approach.