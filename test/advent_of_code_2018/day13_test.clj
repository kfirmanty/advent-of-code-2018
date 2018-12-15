(ns advent-of-code-2018.day13-test
  (:require [advent-of-code-2018.day13 :as d]
            [clojure.test :refer :all]))

(defn ->cart [[x y]]
  (d/new-cart x y \>))

(defn load-map [name]
  (-> (str "resources/day13" name) slurp d/input->map))

(deftest should-order-properly
  (let [positions [[0 0] [1 0] [2 0]
                   [0 1] [1 1] [2 1]]
        state {:carts (->> positions
                           (map ->cart)
                           (map #(vector (:id %) %))
                           (into {}))}]
    (is (= [[0 0] [1 0] [2 0]
            [0 1] [1 1] [2 1]]
           (->> state d/carts-in-order (map :pos))))))

(deftest simple-crash-test
  (let [crash-map (load-map "simplecrash")
        crash-map-horizontal  (load-map "simplecrashhorizontal")
        crash (d/solve-1 crash-map)
        crash-horizontal (d/solve-1 crash-map-horizontal)]
    (is (= [0 3] crash))
    (is (= [3 0] crash-horizontal))))

(deftest should-crash-in-order
  (let [crash-map (d/input->map "|
|
v
^
|")
        crash-map-horizontal  (d/input->map "--><--")
        crash (d/solve-1 crash-map)
        crash-horizontal (d/solve-1 crash-map-horizontal)]
    (is (= [0 3] crash))
    (is (= [3 0] crash-horizontal))))

(deftest should-pass-test-description
  (let [state (load-map "")
        crash (d/solve-1 state)]
    (is (= [7 3] crash))))

(deftest turns-test
  (let [turn-map (load-map "turnmap")
        state (-> turn-map d/tick d/tick)
        positions (->> state :carts vals (map :pos) sort)
        proper-positions (sort [[0 1] [1 3] [3 3] [4 1]
                                [1 5] [0 7] [4 7] [3 5]])]
    (is (nil? (:crash positions)))
    (is (= proper-positions positions))))

(deftest crossroads-test
  (let [cross-map (load-map "crossroadsmap")
        state (-> cross-map d/tick d/tick d/tick d/tick)
        cart (-> state :carts vals first)]
    (= [2 0] (:pos cart))))

(deftest should-pass-input-1
  (is (= [8 3] (d/solve-1))))

(deftest should-pass-description-2
  (let [pos (-> "desc2" load-map d/solve-2)]
    (is (= [6 4] pos))))

(deftest should-pass-input-2
  (let [pos (d/solve-2)]
    (is (= [73 121] pos))))
