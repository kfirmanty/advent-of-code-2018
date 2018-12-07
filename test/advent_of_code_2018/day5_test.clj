(ns advent-of-code-2018.day5-test
  (:require [advent-of-code-2018.day5 :as d]
            [clojure.test :refer :all]))

(deftest should-consume-chars
  (is (d/gulp? \a \A))
  (is (d/gulp? \A \a)))

(deftest description-tests
  (is (.isEmpty (d/consume "aA")))
  (is (.isEmpty (d/consume "abBA")))
  (is (= "abAB" (d/consume "abAB")))
  (is (= "aabAAB" (d/consume "aabAAB"))))

(deftest description-tests-mutable
  (is (.isEmpty (d/consume-mutable "aA")))
  (is (.isEmpty (d/consume-mutable "abBA")))
  (is (= "abAB" (d/consume-mutable "abAB")))
  (is (= "aabAAB" (d/consume-mutable "aabAAB"))))
