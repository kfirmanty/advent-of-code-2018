(ns advent-of-code-2018.day15
  (:require [advent-of-code-2018.common :as c]))

;;
;;tile
{:pos [0 0]
 :type #{:wall :open}}

;;entity
{:pos [0 0]
 :id ""
 :attack 3
 :hp 200
 :type #{:elf :goblin}}

(defn add-entity [state {:keys [pos] :as entity}]
  (-> state
      (assoc-in [:entities (:id entity)] entity)
      (assoc-in [:entities-xy (first pos) (second pos)] entity)))

(defn get-entities [state]
  (->> state :entities vals))

(defn get-entity [state id]
  (get-in state [:entities id]))

(defn remove-entity [state {:keys [pos] :as entity}]
  (when (nil? pos) (throw (Exception. (str (:entities state)))))
  (-> state
      (update :entities dissoc (:id entity))
      ;;(update-in [:entities-xy (first pos)] dissoc (second pos))
      ))

(defn update-entity [state {:keys [pos] :as entity}]
  (-> state
      ;;(remove-entity (get-entity state (:id entity)))
      (assoc-in [:entities (:id entity)] entity)
      ;;(assoc-in [:entities-xy (first pos) (second pos)] entity)
      ))

(defn tile-at [state x y]
  (get-in state [:tiles x y]))

(defn entity-at [state x y]
  ;;(not-empty (get-in state [:entities-xy x y]))
  (->> state get-entities (filter #(= [x y] (:pos %))) first)
  )

(defn entity-of-type-at [state x y type]
  (let [e (entity-at state x y)]
    (when (and e (= type (:type e)))
      e)))

(defn entities-in-order [state]
  (sort c/aoc-comparator (get-entities state)))

(defn targets [state type]
  (->> state get-entities (filter #(= type (:type %)))))

(defn state->str [s]
  (let [max-x (->> s :tiles keys sort last inc)
        max-y (->> s :tiles vals (map keys) flatten sort last inc)
        entities (->> s get-entities (reduce (fn [s c]
                                               (let [[x y] (:pos c)]
                                                 (assoc-in s [x y] c))) {}))
        sb (StringBuilder.)]
    (doseq [y (range max-y)]
      (doseq [x (range max-x)]
        (let [t (get-in s [:tiles x y])
              entity (get-in entities [x y])]
          (cond
            (= :elf (:type entity)) (.append sb "E")
            (= :goblin (:type entity)) (.append sb "G")
            (= :wall (:type t)) (.append sb "#")
            :else (.append sb "."))))
      (.append sb "\n"))
    (.toString sb)))

(defn print-state [s]
  (println)
  (println (state->str s)))

(defn entity [x y c attack-power]
  {:pos [x y]
   :id (.toString (java.util.UUID/randomUUID))
   :type (if (= c \E)
           :elf
           :goblin)
   :attack (if (= c \E)
             attack-power
             3)
   :hp 200})

(defn tile [x y c]
  {:pos [x y]
   :type (if (= c \.)
           :open
           :wall)})

(defn parse-line [l y attack-power]
  (for [x (range (count l))]
    (let [c (nth l x)]
      (cond
        (#{\E \G} c) [(entity x y c attack-power)
                      (tile x y \.)]
        (#{\. \#} c) (tile x y c)))))

(defn parse
  ([i]
   (parse i 3))
  ([i attack-power]
   (let [lines (clojure.string/split i #"\n")]
     (->> (for [y (range (count lines))]
            (parse-line (nth lines y) y attack-power))
          flatten
          (reduce (fn [s el] (let [[x y] (:pos el)]
                               (if (:id el)
                                 (add-entity s el)
                                 (assoc-in s [:tiles x y] el)))) {})))))

(defn target-type [entity]
  (if (= (:type entity) :elf)
    :goblin
    :elf))

(defn most-suitable [enemies]
  (let [sorted (sort-by :hp enemies)
        part-by-hp (partition-by :hp sorted)]
    (if (> (-> part-by-hp first count) 1)
      (first (sort c/aoc-comparator (first part-by-hp)))
      (first sorted))))

(defn attack [state entity enemies]
  (let [enemy (most-suitable enemies)
        enemy-hp (- (:hp enemy) (:attack entity))]
    (if (<= enemy-hp 0)
      (remove-entity state enemy)
      (update-entity state (assoc enemy :hp enemy-hp)))))

(defn in-attack-range? [entity enemy]
  (->>  (map - (:pos entity) (:pos enemy))
        (map #(Math/abs %))
        (reduce + 0)
        (= 1)))

(defn possible-to-attack [state entity]
  (let [pos (:pos entity)]
    (filter #(in-attack-range? entity %)
            (targets state (target-type entity)))))

(def dirs
  [[0 1] [0 -1] [1 0] [-1 0]])

(defn wall? [state [x y]]
  (= :wall (:type (tile-at state x y))))

(defn possible-positions [state type history [x y]]
  (->> dirs
       (map #(vector (+ x (first %)) (+ y (second %))))
       (filter #(and (not (contains? history %))
                     (not (wall? state %))
                     (not (entity-of-type-at state (first %) (second %) type))))))

(defn find-solved [paths]
  (filter #(= (last %) :solved) paths))

(defn print-debug-map [debug-map]
  (println)
  (doseq [y (range (count (aget debug-map 0)))]
    (doseq [x (range (count debug-map))]
      (print (aget debug-map x y)))
    (println))
  (println))

(defn init-arr [state]
  (let [x-size (->> state :tiles keys sort last inc)
        y-size (->> state :tiles vals (map keys) flatten sort last inc)]
    (let [arr (for [x (range x-size)]
                (for [y (range y-size)]
                  0))]
      (to-array-2d arr))))

(defn new-paths [state paths type enemy-type history]
  (let [llist (java.util.LinkedList.)]
    (doseq [path paths]
      (let [possible (possible-positions state type @history (last path))
            with-enemies (doall (->> possible
                                     (map #(entity-of-type-at state (first %) (second %) enemy-type))
                                     (filter some?)))]
        (cond (empty? possible) :dead-end
              (not-empty with-enemies) (.add llist :solved)
              :else (doall (map (fn [p]
                                  (.add llist (conj path (into [] p)))
                                  (swap! history conj p)) possible)))))
    llist))

(defn move [state pos type enemy-type prev-pos]
  (let [history (atom #{pos prev-pos})
        paths [[pos]]]
    (loop [paths (new-paths state paths type enemy-type history) i 0]
      (let [solved (filter #(= % :solved) paths)]
        (cond (not-empty solved) [:solved i]
              (empty? paths) [:dead-end i]
              :else (recur (new-paths state paths type enemy-type history) (inc i)))))))

(defn new-pos [state entity]
  (if (not-empty (possible-to-attack state entity))
    (:pos entity)
    (let [near-locations (possible-positions state (:type entity) #{} (:pos entity))
          best-move (->> near-locations (map #(vector %
                                                      (move state % (:type entity) (target-type entity) (:pos entity))))
                         (filter #(= :solved (-> % second first))) ;; example structure: [[2 2] [:solved 3]]
                         (sort-by (comp second second))
                         (partition-by (comp second second))
                         first
                         (map first)
                         (sort c/pos-compare)
                         first)]
      (if (nil? best-move)
        (:pos entity)
        best-move))))

(defn tick-entity [state entity]
  (let [pos (new-pos state entity)
        state (update-entity state (assoc entity :pos pos))
        entity (get-entity state (:id entity))
        enemies (possible-to-attack state entity)
        finished? (empty? (targets state (target-type entity)))]
    (if (not-empty enemies)
      (update-entity (attack state entity enemies) entity)
      (if finished?
        (assoc state :shorter-turn? true :finished? true)
        (update-entity state entity)))))

(defn tick [state]
  (loop [state state taken-turn #{}]
    (let [entity (first (drop-while #(taken-turn (:id %)) (entities-in-order state)))]
      (if (nil? entity)
        (cond (:finished? state)
              state
              (or (empty? (targets state :goblin)) (empty? (targets state :elf)))
              (assoc state :finished? true)
              :else
              state)
        (recur (tick-entity state entity) (conj taken-turn (:id entity)))))))

(defn solve-loop [state]
  (loop [state (assoc state :rounds 0)]
    (if (:finished? state)
      (update state :rounds #(if (:shorter-turn? state) (dec %) %))
      (recur (update (tick state) :rounds inc)))))

(defn ->score [state]
  (->> state get-entities (reduce #(+ %1 (:hp %2)) 0) (* (:rounds state))))

(defn solve-1 []
  (let [state (-> "resources/day15input" slurp parse solve-loop)]
    (->score state)))

(defn all-elves [state]
  (->> state get-entities (filter #(= :elf (:type %)))))

(defn solve-2 []
  (let [state-str (slurp "resources/day15input")
        elves-count (-> state-str parse all-elves count)]
    (loop [attack-power 4]
      (let [state (solve-loop (parse state-str attack-power))
            elves (-> state all-elves count)]
        (if (= elves elves-count)
          (->score state)
          (recur (inc attack-power)))))))
