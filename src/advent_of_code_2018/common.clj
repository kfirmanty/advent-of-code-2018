(ns advent-of-code-2018.common
  (:require [instaparse.core :as insta]))

(defn parse-input [i parser]
  (->> (clojure.string/split i #"\n")
       (map parser)
       (map (fn [p] (insta/transform {:number #(Integer/parseInt %)} p)))
       (map #(into {} %))))

(defn pos-compare [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (cond (< y1 y2) -1
          (> y1 y2) 1
          (< x1 x2) -1
          (> x1 x2) 1
          :else 0)))

(defn aoc-comparator [e1 e2]
  (pos-compare (:pos e1) (:pos e2)))
