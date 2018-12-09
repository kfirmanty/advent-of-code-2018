(ns advent-of-code-2018.day8-test
  (:require [advent-of-code-2018.day8 :as d]
            [clojure.test :refer :all]))

(deftest should-parse-without-children
  (let [p (d/solve-1 [0 4 1 2 3 4])]
    (println p)
    (is (= 4 (count (:meta p))))))

(deftest should-parse-simple-structure
  (let [p (d/solve-1 [2 1
                      2 1
                      0 1 1
                      0 1 1
                      1
                      0 1 1
                      1])]
    (println p)
    (is (= 2 (count (:children p))))
    (is (= 2 (count (-> p :children first :childre))))))

(deftest should-parse-test-desc
  (let [p (d/solve-1 [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])]
    (is (= 2 (count (:children p))))
    (is (= 138 (d/calculate-meta p)))))
