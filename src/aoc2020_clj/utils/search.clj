(ns aoc2020-clj.utils.search
  (:require [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(defn a-star-search
  [start neighbor-func goal? remain-cost path-cost]
  (loop [q (conj (sorted-set) [0 start])
         cost-so-far {start 0}
         came-from   {start nil}]
    (if-let [[node-cost node :as node-state] (first q)]
      (if (goal? node)
        (reverse (take-while (complement nil?) (iterate came-from node)))
        (let [neighbors (neighbor-func node)
              prev-cost (cost-so-far node)
              cheaper (remove #(< (cost-so-far % Double/POSITIVE_INFINITY)
                                  (+ prev-cost (path-cost node %)))
                              neighbors)
              new-nodes (map #(vector (+ prev-cost
                                         (path-cost node %)
                                         (remain-cost %)) %)
                             cheaper)]
          (recur (into (disj q node-state) new-nodes)
                 (->> cheaper
                      (map #(vector % (+ prev-cost (path-cost node %))))
                      (into cost-so-far))
                 (into came-from (map vector cheaper (repeat node))))))
      cost-so-far)))

(defn graph-search [search-type start next-moves is-goal?]
  {:pre (contains? #{:dfs :bfs} search-type)}
  (let [q (search-type {:bfs (PersistentQueue/EMPTY) :dfs []})]
    (loop [open (conj q start)
           closed #{}]
      (cond (is-goal? (peek open)) (peek open)
            (closed (peek open)) (recur (pop open) closed)
            :else (recur (into (pop open) (next-moves (peek open)))
                         (conj closed (peek open)))))))

(defn breadth-first-graph-search
  [start next-moves is-goal?]
  (graph-search :bfs start next-moves is-goal?))

(defn depth-first-graph-search
  [start next-moves is-goal?]
  (graph-search :dfs start next-moves is-goal?))

(defn schedule
  [& {:keys [k lam limit] :or {k 20 lam 0.005 limit 100}}]
  (fn [t] (if (< t limit) (* k (Math/exp (* t (- lam) ))) 0)))

(defn simulated-annealing
  "Randomly select neighbors to move to, with decreasing frequency as
  temperature drops. Temperature should  decrease over time, and as that
  happens the resultant value of e^-delta/temp will approach zero.
  Finally, once the value of e^-delta/temp is determined, it is compared
  to a uniformly random number between 0 and 1.  If the value exceeds this
  random number, we switch.

  Example of how temperature impacts probability of switching, holding delta
  constant:
    exp(-0.01) ~= 0.99  - almost certainly switch (likely > random(0,1))
    exp(-0.99) ~= 0.37  - likely not switch (likely < rand(0,1))
  One useful heuristic for setting an initial temperature is to estimate
  the maximum absolute delta of the cost-fn.  In this implementation, that
  temperature is represented by the constant 'k' to the schedule function."
  [start get-neighbors cost-fn schedule goal?]
  (loop [node start iter 1]
    (let [old-cost (cost-fn node)
          neighbors (-> node get-neighbors vec)
          temp (schedule iter)] ;; get exponentially smaller
      (cond (or (empty? neighbors) (zero? temp)) ["so-far" node]
            (goal? node) ["solution" node]
            :else (let [neighbor (rand-nth neighbors)
                        new-cost (cost-fn neighbor)
                        delta    (- new-cost old-cost)]
                    (println old-cost new-cost delta temp (Math/exp (/ (- delta) temp)))
                    (recur (if (or (< delta 0)
                                   (< (rand 1.0) (Math/exp (/ (- delta) temp))))
                             neighbor
                             node)
                           (inc iter)))))))

(defn random-search
  "take the best move we can at every step; if none better quit"
  [start get-neighbors cost-fn goal? max-tries]
  (loop [node start i 0]
    (if (or (> i max-tries) (empty? (get-neighbors node)))
      {:node node :iteration i}
      (recur (rand-nth (-> node get-neighbors vec)) (inc i)))))

(defn hill-climb
  "take the best move we can at every step; if none better quit"
  [start get-neighbors cost-fn goal? max-tries]
  (loop [node start i 1]
    (println (count node) node)
    (if (or (> i max-tries)
            (empty? (get-neighbors node))
            (goal? node))
      {:node node :completed? false :iteration i}
      (let [neighbor (apply min-key cost-fn (get-neighbors node))
            old-cost (cost-fn node)
            new-cost (cost-fn neighbor)]
        (println (count (get-neighbors node)))
        (if (<= new-cost old-cost)
          (recur neighbor (inc i))
          {:node node :completed? true :iteration i})))))
#_(def graph {:s {:a 1 :b 4}
            :a {:b 2 :c 5 :g 12}
            :b {:c 2}
            :c {:g 3}})

#_(def h-vals {:s 7 :a 6 :b 2 :c 1 :g 0})

#_(a-star-search :s
               #(-> % graph keys)
               #(= :g %)
               h-vals
               (fn [from to] (get-in graph [from to])))