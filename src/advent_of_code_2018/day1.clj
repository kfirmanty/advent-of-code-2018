(ns advent-of-code-2018.day1)

(defn parse-input [i]
  (->> (clojure.string/split i #"\n")
       (map #(Integer/valueOf %))))

(defn main-a [i]
  (->> i
       parse-input
       (reduce +)))

(defn find-frequency-twice [i]
  (loop [history #{0} x (first i) xs (rest i) s 0]
    (let [f (+ x s)
          in-history? (history f)]
      (if in-history?
        f
        (recur (conj history f) (first xs) (rest xs) f)))))

(defn main-b [i]
  (find-frequency-twice (-> i parse-input cycle)))
