(ns advent-of-code-2018.day8)

(defn parse [input node]
  (let [[c m] (take 2 @input)]
    (swap! input #(drop 2 %))
    (if (empty? @input)
      node
      (if (= 0 c)
        (let [meta (take m @input)]
          (swap! input #(drop m %))
          (assoc node :meta meta))
        (let [node (loop [i 0 node node]
                     (if (= i c)
                       node
                       (let [child-node {:children [] :meta []}]
                         (recur (inc i) (update node :children conj (parse input child-node))))))]
          (let [meta (take m @input)]
            (swap! input #(drop m %))
            (assoc node :meta meta)))))))

(defn calculate-meta [p]
  (+ (reduce + (:meta p))
     (reduce + (map calculate-meta (:children p)))))

(defn indexed-meta [i metas]
  (if (and (>= i 0) (< i (count metas)))
    (nth metas i)
    0))

(defn calculate-meta-2 [p]
  (if (empty? (:children p))
    (reduce + (:meta p))
    (let [children-meta (map calculate-meta-2 (:children p))]
      (reduce + (map #(indexed-meta (dec %) children-meta) (:meta p))))))

(defn solve-1
  ([i]
   (-> i atom (parse {:children [] :meta []})))
  ([]
   (solve-1
    (map #(Integer/parseInt %) (clojure.string/split (slurp "resources/day8") #" ")))))
