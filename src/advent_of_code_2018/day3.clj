(ns advent-of-code-2018.day3
  (:require [instaparse.core :as insta]
            [advent-of-code-2018.common :as common]))

(def input-parser (insta/parser
                   "
<S>=<'#'> claim <ws> <'@'> <ws> x <','> y <':'> <ws> width <'x'> height
claim=number
width=number
height=number
x=number
y=number
number=#'\\d+'
ws=#'\\s+'"))

(defn squares-claimed [{:keys [width height x y claim]}]
  (for [x (range x (+ x width))
        y (range y (+ y height))]
    [x y claim]))

(defn pixels [is]
  (->> is
       (map squares-claimed)
       (reduce concat)
       (group-by (fn [[x y _]] [x y]))
       vals))

(defn already-claimed? [v]
  (> (count v) 1))

(defn solve-1 [i]
  (let [pixels (-> i (common/parse-input input-parser) pixels)
        claimed (->> pixels (map #(if (already-claimed? %)
                                    1
                                    0))
                     (reduce +))]
    claimed))

(defn third [v]
  (nth v 2))

(defn solve-2 [i]
  (let [parsed (common/parse-input i input-parser)
        claims (->> parsed (map :claim)
                        (into #{}))
        overlapping (->> parsed pixels
                             (map #(if (already-claimed? %) (map third %) nil))
                             (filter some?)
                             flatten
                             (into #{}))]
    (clojure.set/difference claims overlapping)))
