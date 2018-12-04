(ns advent-of-code-2018.common
  (:require [instaparse.core :as insta]))

(defn parse-input [i parser]
  (->> (clojure.string/split i #"\n")
       (map parser)
       (map (fn [p] (insta/transform {:number #(Integer/parseInt %)} p)))
       (map #(into {} %))))
