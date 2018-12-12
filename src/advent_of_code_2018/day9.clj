(ns advent-of-code-2018.day9)

(defn pop-marble! [board]
  (.pop board))

(defn move-board! [board dist]
  (let [swapfn (if (< dist 0)
                 #(.addFirst board (.removeLast board))
                 #(.addLast board (.removeFirst board)))
        abs-dist (Math/abs dist)]
    (doall (repeatedly abs-dist swapfn))))

(defn insert-marble! [board marble]
  (.push board marble))

(defn on-multiple [{:keys [player marble board] :as state}]
  (move-board! board -7)
  (-> state
      (update-in [:scores player] + marble)
      (update-in [:scores player] + (pop-marble! board))))

(defn on-normal [{:keys [marble board] :as state}]
  (move-board! board 2)
  (insert-marble! board marble)
  state)

(defn update-state [state]
  (let [multiple? (= 0 (mod (:marble state) 23))
        state (if multiple?
                (on-multiple state)
                (on-normal state))]
    (-> state
        (update :player #(mod (inc %) (:players-num state)))
        (update :marble inc))))

(defn iterate-state [players last-marble]
  (loop [state (update-state {:board (doto (java.util.LinkedList.)
                                       (.add 0))
                              :pos 0
                              :player 0
                              :players-num players
                              :scores (->> (range 0 players)
                                           (map #(vector % 0))
                                           (into {}))
                              :marble 1})]
    (if (> (:marble state) last-marble)
      state
      (recur (update-state state)))))

(defn solve-1 [players last-marble]
  (->> (iterate-state players last-marble)
       :scores
       (apply max-key second)
       second))
