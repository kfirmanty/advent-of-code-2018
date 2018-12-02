(ns advent-of-code-2018.day2)

(defn repetitions [id]
  (->> id
       frequencies
       vals
       (into #{})))

(defn n-of-any-letter [id n]
  (let [repetitions (repetitions id)]
    (if (repetitions n)
      1
      0)))

(defn id-val [id]
  [(n-of-any-letter id 2) (n-of-any-letter id 3)])

(defn checksum [ids]
  (->> ids
       (map id-val)
       (reduce (fn [s v] (map + s v)) [0 0])
       (apply *)))

(defn parse-input [i]
  (clojure.string/split i #"\n"))

(defn main-1 [i]
  (-> i
      parse-input
      checksum))

(defn id-distance [id1 id2]
  (->> (map #(if (= %1 %2) 0 1) id1 id2)
       (reduce +)))

(defn without-diff [id1 id2]
  (apply str (map #(when (= %1 %2) %1) id1 id2)))

(defn find-close [id ids]
  (some (fn [id2]
          (let [distance (id-distance id id2)]
            (when (= distance 1) (without-diff id id2)))) ids))

(defn main-2 [i]
  (let [ids (parse-input i)]
    (loop [id (first ids) rids (rest ids)]
      (let [close (find-close id rids)]
        (if close
          close
          (recur (first rids) (rest rids)))))))
