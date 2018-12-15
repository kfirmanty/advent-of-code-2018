(ns advent-of-code-2018.day13)

{:carts [{:dir [0 1]
          :pos [0 0]
          :next-turn #{:left :straight :right}}]}

{:tracks {:pos [0 0]
          :type #{:track :intersection}
          :char \|}}

(defn char->dir [c]
  (condp = c
    \< [-1 0]
    \> [1 0]
    \v [0 1]
    \^ [0 -1]))

(defn new-cart [x y init-char]
  {:dir (char->dir init-char)
   :pos [x y]
   :next-turn :left
   :id (.toString (java.util.UUID/randomUUID))})

(defn parse-line [l y]
  (for [x (range (count l))]
    (let [c (.charAt l x)]
      (cond (#{\| \-} c) {:pos [x y]
                          :type :track
                          :char c}
            (#{\+ \/ \\ } c) {:pos [x y]
                              :type :intersection
                              :char c}
            (#{\< \> \v \^} c) [(new-cart x y c)
                                {:pos [x y]
                                 :type :track
                                 :char (if (or (= c \v) (= c \^))
                                         \|
                                         \-)}]
            :else nil))))

(defn input->map [i]
  (let [lines  (clojure.string/split i #"\n")
        parsed (for [y (range (count lines))]
                 (parse-line (nth lines y) y))]
    (->> parsed
         flatten
         (filter some?)
         (reduce (fn [s el]
                   (let [[x y] (:pos el)]
                     (if (:id el)
                       (assoc-in s [:carts (:id el)] el)
                       (assoc-in s [:tracks x y] el)))) {}))))

(defn check-for-crash [state]
  (let [grouped (group-by :pos (vals (:carts state)))
        crash (->> grouped vals (filter #(> (count %) 1)))]
    (if (not-empty crash)
      (let [crashes-ids (->> crash flatten (map :id))]
        (-> state
            (assoc :crash (first crash))
            (update :carts #(apply dissoc % crashes-ids))))
      state)))

(defn tile-at [state x y]
  (get-in state [:tracks x y]))

(defn swap-xy [[x y]]
  [y x])

(defn resolve-normal-dir [{:keys [char]} {:keys [dir]}]
  (condp = char
    \\ (swap-xy dir)
    \/ (map (partial * -1) (swap-xy dir))
    dir))

(def next-turn
  {:left :straight
   :straight :right
   :right :left})

(defn dir-rot [dir next-turn]
  (let [rots
        {:left {[1 0] [0 -1]
                [-1 0] [0 1]
                [0 -1] [-1 0]
                [0 1] [1 0]}
         :right {[1 0] [0 1]
                 [-1 0] [0 -1]
                 [0 1] [-1 0]
                 [0 -1] [1 0]}}]
    (if (= :straight next-turn)
      dir
      (get-in rots [next-turn dir]))))

(defn resolve-crossroads [{:keys [dir next-turn]}]
  (dir-rot dir next-turn))

(defn move [state cart]
  (if-not (get-in state [:carts (:id cart)])
    state
    (let [[nx ny] (map + (:pos cart) (:dir cart))
          tile (tile-at state nx ny)
          ncart (if (= (:char tile) \+)
                  (assoc cart :pos [nx ny]
                         :dir (resolve-crossroads cart)
                         :next-turn (next-turn (:next-turn cart)))
                  (assoc cart :pos [nx ny]
                         :dir (resolve-normal-dir tile cart)))]
      (assoc-in state [:carts (:id cart)] ncart))))

(defn carts-in-order [state]
  (sort (fn [c1 c2] (let [[x1 y1] (:pos c1)
                          [x2 y2] (:pos c2)]
                      (cond (< y1 y2) -1
                            (> y1 y2) 1
                            (< x1 x2) -1
                            (> x1 x2) 1
                            :else 0))) (vals (:carts state))))

(defn tick [state]
  (let [sorted (carts-in-order state)]
    (loop [state state c (first sorted) cs (rest sorted)]
      (if (or (nil? c) (:crash state))
        state
        (recur (-> state
                   (move c)
                   check-for-crash)
               (first cs)
               (rest cs))))))

(defn tick-2 [state]
  (let [sorted (carts-in-order state)]
    (loop [state state c (first sorted) cs (rest sorted)]
      (if (nil? c)
        state
        (recur (-> state
                   (move c)
                   check-for-crash)
               (first cs)
               (rest cs))))))

(defn solve-2
  ([]
   (solve-2 (-> "resources/day13task" slurp input->map)))
  ([state]
   (loop [state state]
     (if (= 1 (-> state :carts vals count))
       (-> state :carts vals first :pos)
       (recur (tick-2 state))))))

(defn solve-1
  ([]
   (solve-1 (-> "resources/day13task" slurp input->map)))
  ([state]
   (loop [state state]
     (if (:crash state)
       (-> state :crash first :pos)
       (recur (tick state))))))

(defn print-state [s]
  (let [max-x (->> s :tracks keys sort last inc)
        max-y (->> s :tracks vals (map keys) flatten sort last inc)
        carts (->> s :carts vals (reduce (fn [s c]
                                           (let [[x y] (:pos c)]
                                             (assoc-in s [x y] c))) {}))]
    (println)
    (doseq [y (range max-y)]
      (doseq [x (range max-x)]
        (let [c (get-in s [:tracks x y :char])
              cart (get-in carts [x y])]
          (cond
            cart (print "#")
            c (print c)
            :else (print " "))))
      (println))))
