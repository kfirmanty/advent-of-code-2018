(ns advent-of-code-2018.day15-test
  (:require [advent-of-code-2018.day15 :as d]
            [clojure.test :refer :all]))

(defn load-map [name]
  (->> name (str "resources/day15") slurp d/parse))

(defn find-entities [state type]
  (->> state d/get-entities (filter #(= (:type %) type))))

(defn find-entity [state type]
  (first (find-entities state type)))

(defn find-elf [state]
  (find-entity state :elf))

(defn find-goblin [state]
  (find-entity state :goblin))

(deftest state-changes
  (let [state (load-map "simple")
        elf (find-elf state)
        state (d/update-entity state (assoc elf :hp 197))
        elf (find-elf state)
        state (d/update-entity state (assoc elf :pos [10 10]))]
    (is (= 197 (:hp (d/entity-at state 10 10))))))

(deftest should-move-toward-enemy
  (let [state (load-map "simple")
        elf (find-elf state)
        state (d/tick-entity state elf)]
    (= [2 1] (:pos (find-elf state)))))

(deftest should-order-entites
  (let [input "#######\n#.G.E.#\n#E.G.E#\n#.G.E.#\n#######"
        state (d/parse input)
        in-order (d/entities-in-order state)]
    (is (= [[2 1] [4 1] [1 2] [3 2] [5 2] [2 3] [4 3]]) (map :pos in-order))))

(deftest should-attack-elf
  (let [state (-> "simpleattack" load-map d/solve-loop)]
    (is (some? (find-elf state)))
    (is (nil? (find-goblin state)))
    (is (= 67 (:rounds state)))))

(deftest should-attack-above
  (let [state (load-map "attackorder")
        elf (find-elf state)
        state (d/tick-entity state elf)]
    (is (= 197(:hp (d/entity-at state 2 1)))))
  (let [state (load-map "attackorder2")
        elf (find-elf state)
        state (d/tick-entity state elf)]
    (is (= 197 (:hp (d/entity-at state 1 1))))))

(deftest should-select-with-ordering-move
  (let [state (load-map "moveordering")
        elf (find-elf state)
        move-path (d/new-pos state elf)]
    (is (= [3 1] move-path))))

(deftest should-pass-desc
  (let [state (-> "desc" load-map d/solve-loop)]
    (is (= 47 (:rounds state)))
    (is (= 27730 (d/->score state)))
    (is (= 200 (:hp (d/entity-at state 1 1))))
    (is (= 131 (:hp (d/entity-at state 2 2))))
    (is (= 59 (:hp (d/entity-at state 5 3))))
    (is (= 200 (:hp (d/entity-at state 5 5))))))

(defn trim-out [out]
  (.replace (.trim out) "\n" ""))

(deftest should-pass-movement-test
  (let [state (-> "largermovement" load-map d/tick d/tick d/tick)
        out (d/state->str state)
        oute (slurp "resources/day15largermovemente")]
    (println out)
    (println oute)
    (is (= (trim-out oute) (trim-out out)))))

(defn trim-out [out]
  (.replace (.trim out) "\n" ""))

(defn compare-outs [out1 out2]
  (is (= (trim-out out1) (trim-out out2))))

(deftest should-have-same-outcomes
  (let [sol1 (-> "sol1" load-map d/solve-loop)
        out1e (slurp "resources/day15sol1e")
        out1 (d/state->str sol1)]
    (is (= (trim-out out1e) (trim-out out1)))
    (is (= 37 (:rounds sol1)))
    (is (= 200 (:hp (d/entity-at sol1 5 1))))
    (is (= 197 (:hp (d/entity-at sol1 1 2))))
    (is (= 185 (:hp (d/entity-at sol1 2 3))))
    (is (= 200 (:hp (d/entity-at sol1 1 4))))
    (is (= 200 (:hp (d/entity-at sol1 5 4))))
    (is (= 36334 (d/->score sol1))))
  (let [sol2 (-> "sol2" load-map d/solve-loop)
        out2e (slurp "resources/day15sol2e")
        out2 (d/state->str sol2)]
    (is (= (trim-out out2e) (trim-out out2)))
    (is (= 46 (:rounds sol2)))
    (is (= 164 (:hp (d/entity-at sol2 2 1))))
    (is (= 197 (:hp (d/entity-at sol2 4 1))))
    (is (= 200 (:hp (d/entity-at sol2 3 2))))
    (is (= 98 (:hp (d/entity-at sol2 1 3))))
    (is (= 200 (:hp (d/entity-at sol2 2 4))))
    (is (= 39514 (d/->score sol2))))
  
  (let [sol3 (-> "sol3" load-map d/solve-loop)
        out3e (slurp "resources/day15sol3e")
        out3 (d/state->str sol3)]
    (is (= (trim-out out3e) (trim-out out3)))
    (is (= 35 (:rounds sol3)))
    (is (= 200 (:hp (d/entity-at sol3 1 1))))
    (is (= 98 (:hp (d/entity-at sol3 3 1))))
    (is (= 200 (:hp (d/entity-at sol3 3 2))))
    (is (= 95 (:hp (d/entity-at sol3 5 4))))
    (is (= 200 (:hp (d/entity-at sol3 4 5))))
    (is (= 27755 (d/->score sol3))))

  (let [sol4 (-> "sol4" load-map d/solve-loop)
        out4e (slurp "resources/day15sol4e")
        out4 (d/state->str sol4)]
    (is (= (trim-out out4e) (trim-out out4)))
    (is (= 54 (:rounds sol4)))
    (is (= 200 (:hp (d/entity-at sol4 3 2))))
    (is (= 98 (:hp (d/entity-at sol4 1 5))))
    (is (= 38 (:hp (d/entity-at sol4 3 5))))
    (is (= 200 (:hp (d/entity-at sol4 5 5))))
    (is (= 28944 (d/->score sol4))))

  (let [sol5 (-> "sol5" load-map d/solve-loop)
        out5e (slurp "resources/day15sol5e")
        out5 (d/state->str sol5)]
    (is (= (trim-out out5e) (trim-out out5)))
    (is (= 20 (:rounds sol5)))
    (is (= 137 (:hp (d/entity-at sol5 2 1))))
    (is (= 200 (:hp (d/entity-at sol5 1 2))))
    (is (= 200 (:hp (d/entity-at sol5 3 2))))
    (is (= 200 (:hp (d/entity-at sol5 2 3))))
    (is (= 200 (:hp (d/entity-at sol5 2 5))))
    (is (= 18740 (d/->score sol5)))))

(deftest proper-move-in-big-map
    (let [state (load-map "bigmap")
          elf (find-elf state)
          state (d/tick-entity state elf)]
      (= [3 2] (:pos (find-elf state)))))

(deftest should-solve-part-1
  (let [init-state (load-map "input")
        sol (d/solve-loop init-state)
        hp (->> sol d/get-entities (reduce #(+ %1 (:hp %2)) 0))]
    (is (= 2606 hp))
    (is (= 82 (:rounds sol)))
    (is (= 213692 (d/->score sol)))))

(deftest should-solve-part-2
  (is (= 52688 (d/solve-2))))
