(ns advent-of-code-2018.day5)

(defn gulp? [x1 x2]
  (and (= (Character/toUpperCase x1) (Character/toUpperCase x2))
       (not= x1 x2)))

(defn consume [i]
  (loop [[x1 x2] (take 2 i) front [] back (drop 2 i)]
    (if (or (nil? x1) (nil? x2))
      (apply str (concat front [x1 x2] back))
      (if (gulp? x1 x2)
        (if (empty? front)
          (recur (take 2 back) front (drop 2 back))
          (recur [(last front) (first back)] (into [] (drop-last front)) (rest back)))
        (recur [x2 (first back)] (conj front x1) (rest back))))))

(defn consume-mutable [input]
  (let [sb (java.lang.StringBuilder. input)]
    (loop [i 0]
      (if (>= i (dec (.length sb)))
        (.toString sb)
        (let [c (.charAt sb i)
              nc (.charAt sb (inc i))]
          (if (gulp? c nc)
            (do (.deleteCharAt sb i)
                (.deleteCharAt sb i)
                (recur (Math/max (dec i) 0)))
            (recur (inc i))))))))

(defn solve-1 [i]
  (.length (consume-mutable i)))

(defn remove-polymer [i c]
  (.replaceAll (.replaceAll i (str c) "") (.toLowerCase (str c)) ""))

(defn solve-2 [i]
  (let [dist (distinct (.toUpperCase i))
        removed (map (juxt identity #(remove-polymer i %)) dist)]
    (->> removed
         (map (fn [[k i]] [k (solve-1 i)]))
         (apply min-key second))))
