(ns advent-of-code-2018.day9-test
  (:require [advent-of-code-2018.day9 :as d]
            [clojure.test :refer :all]))

(deftest should-have-same-state-as-description
  (let [last-marble 25
        players 9
        state (d/iterate-state players last-marble)]
    (is (= (inc last-marble) (:marble state)))
    (is (= 10 (:pos state)))
    (is (= [0 16  8 17  4 18 19  2 24 20 25 10 21  5 22 11  1 12  6 13  3 14  7 15] (into [] (:board state))) "State after executing 23 multiple should be equal")
    (is (= [0 16  8 17  4 18  9 19  2 20 10 21  5 22 11  1 12  6 13  3 14  7 15] (into [] (:board (d/iterate-state players 22)))) "State before executing 23 multiple should be the same as description")))

(deftest test-index-between
  (let [board [0 1 2 3 4]]
    (is (= 2 (d/index-between board (d/circle-pos board 0 1) (d/circle-pos board 0 2))))))

(deftest should-pass-description-tests
  (is (= 32 (d/solve-1 9 25)))
  (is (= 8317 (d/solve-1 10 1618)))
  (is (= 146373 (d/solve-1 13 7999)))
  (is (= 2764 (d/solve-1 17 1104)))
  (is (= 54718 (d/solve-1 21 6111)))
  (is (= 37305 (d/solve-1 30 5807)))
  (is (= 375414 (d/solve-1 459 71320))))
