(ns aoc2020-clj.day20-2018)

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
N (S|(WE))W = > NSW NEW

(defn parse-dir [parsed]
  (loop [[c & cs] parsed
         cur-prefix ""
         prefix_list []
         paths []]
    (println c cur-prefix prefix_list paths)
    (if-not c
      paths
      (case c
        (\N \E \S \W) (recur cs (str cur-prefix c) prefix_list paths)
        \( (recur cs "" prefix_list (conj paths cur-prefix))
        ;; if next char is `)` then this is an optional path
        \| (recur cs "" (conj prefix_list cur-prefix) paths)
        ;; current options finished, now for each built up path, start from there
        \) (recur cs  "" [] (mapcat (fn [pfix] (map #(str % pfix) paths)) (conj prefix_list cur-prefix)))))))
#_(conj paths (str/join (conj prefix_list cur-prefix)))
#_(mapcat (fn [pfix] (map #(str % pfix) paths)) (conj prefix_list cur-prefix))
;; have a new possible starts list for each group, when group done dump those results into paths
(parse-dir "N(S|E|WW)W(E)")
(parse-dir "ENWWW(NEEE|SSE(EE|N))")
(parse-dir "ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN")

;; recursive descent attempt, handling entire symbols at a time and recursing on lists
(defn choice-point? [instr]
  (some #{\|} (str instr)))

(defn choices [instr]
  (let [instr (str instr)
        instr (if (str/ends-with? instr "|")
                (str instr " ")
                (str instr))]
    (str/split instr #"\|")))

(defn parse-dirs2 [instr]
  #_(println instr)
  (cond (list? instr) (mapcat parse-dirs2 instr)
        (choice-point? instr) (mapcat parse-dirs2 (choices instr))
        :else [(str instr)]))
(parse-dirs2 parsed)


;; open paren -> move stack contents to paths

;;               pop last item from stack
;;               push cur_prefix

;; close paren -> start w/ similar as open paren, maybe not move stack contents?
;; pipe -> push cur_prefix to stack and then reset it
(parse-dir "ENWWW(NEEE|SSE(EE|N))")
;; pfix_stack = [ENWWW] @ '(' : everything in next group starts at peek of this : paths = []
;; pfix_stack = [ENWWW] @ '|' : conj onto paths peek of prefix list + curr_prefix
;; pfix_stack = (conj pfix_stack curr-prefix) => [ENWWW SSE] @ '(' :everything in next group starts at peek of this
;; pfix_stack = [ENWWW SSE] @ '|' conj onto paths join of pfix_list + cur_prfix => [ENWWWNEEE ENWWSSEEE]
;; pfix_stack = [ENWWW SSE] @ ')' conj onto paths join of pfix_list + cur_prfix => [ENWWWNEEE ENWWWSSEEE ENWWWSSEN]
;; pfix_stack = (pop pfix_stack) => ENWWW @ 'nil' bc we recured past end : paths => [ENWWWNEE SSEE SSEN]

;; where we went wrong is those final two on paths should be added to the current (after the final pop) pfix_stack
;; how to fix?



;; at first `)` need to not only set cur-prefix to prev-prefix of `ENNSWW` and continue from there, but also
;; continue from the path formed at that point of `ENNSWWWNEWS`
(parse-dir "ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN")
;; now get coord that you end up with for each sequence, and count steps to get there
(defn follow-dirs [dirs]
  (reduce (fn [acc cur] (mapv + acc cur))
          [0 0]
          (map {\E [0 1] \N [-1 0] \S [1 0] \W [0 -1]}
               dirs)))

(reduce (fn [pos->steps dirs]
          (let [pos (follow-dirs dirs)
                moves (count dirs)]
            (update pos->steps pos (fnil conj []) moves)))
        {}
        (gen-path "" parsed))

(gen-path "" parsed)

