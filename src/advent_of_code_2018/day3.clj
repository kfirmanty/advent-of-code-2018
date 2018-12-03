(ns advent-of-code-2018.day3
  (:require [instaparse.core :as insta]))

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

(defn parse-input [i]
  (->> (clojure.string/split i #"\n")
       (map input-parser)
       (map (fn [p] (insta/transform {:number #(Integer/parseInt %)} p)))
       (map #(into {} %))))

(defn already-claimed? [v]
  (> (count v) 1))

(defn solve-1 [i]
  (let [pixels (-> i parse-input pixels)
        claimed (->> pixels (map #(if (already-claimed? %)
                                    1
                                    0))
                     (reduce +))]
    claimed))

(defn third [v]
  (nth v 2))

(defn solve-2 [i]
  (let [parsed (parse-input i)
        claims (->> parsed (map :claim)
                    (into #{}))
        overlapping (->> parsed pixels
                         (map #(if (already-claimed? %) (map third %) nil))
                         (filter some?)
                         flatten
                         (into #{}))]
    (clojure.set/difference claims overlapping)))
