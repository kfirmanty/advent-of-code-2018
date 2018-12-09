(ns advent-of-code-2018.day7)

(defn parse [s i]
  (let [splited (clojure.string/split i #" ")
        relies-on (nth splited 1)
        step (nth splited 7)]
    (-> s (update :todo conj step relies-on)
        (update-in [:steps step] update :relies-on #(into #{} (conj % relies-on))))))

(defn can-be-done [state]
  (->> (:todo state)
       (filter #(->> % (get (:steps state)) :relies-on empty?))))

(defn do-step [state step]
  (let [steps (->> state :steps
                   (map (fn [[k v]] [k (update v :relies-on disj step)]))
                   (into {}))]
    (-> state
        (update :todo disj step)
        (assoc :steps steps)
        (update :done str step))))

(defn loop-1 [s]
  (loop [s s]
    (if (empty? (:todo s))
      (:done s)
      (recur (do-step s (-> s can-be-done sort first))))))

(defn solve-1 [i]
  (let [parsed (->> (clojure.string/split i #"\n")
                    (reduce parse {:todo #{} :done ""}))]
    (loop-1 parsed)))


(def ^:dynamic base-cost 60)
(defn letter->time [l]
  (+ base-cost (if (string? l)
                 (-> l first int (- 64))
                 (- (int l) 64))))

(defn free-workers [state]
  (->> state :workers vals (filter #(nil? (:doing %)))))

(defn assign-to-worker [state worker task]
  (-> state
      (update :workers #(update % (:id worker) assoc
                                :finishes (+ (:time state) (letter->time task))
                                :doing task))
      (update-in [:steps task] assoc :processed true)))

(defn can-be-done-2 [state]
  (->> (:todo state)
       (filter #(let [step (get (:steps state) %)]
                  (and (-> step :relies-on empty?)
                       (not (:processed step)))))))

(defn finish-work [state worker]
  (-> state
      (update :workers #(update % (:id worker) assoc
                                :finishes 0
                                :doing nil))
      (update-in [:steps (:doing worker)] assoc :processed false)
      (do-step (:doing worker))))

(defn mark-finished [state]
  (let [finished-workers (->> state
                              :workers
                              vals
                              (filter #(and (<= (:finishes %) (:time state))
                                            (some? (:doing %)))))]
    (reduce (fn [state worker] (finish-work state worker)) state finished-workers)))

(defn assign-tasks [state workers steps]
  (loop [state state
         worker (first workers) workers (rest workers)
         step (first steps) steps (rest steps)]
    (if (or (nil? worker) (nil? step))
      state
      (recur (assign-to-worker state worker step)
             (first workers) (rest workers)
             (first steps) (rest steps)))))

(defn loop-2 [state]
  (loop [state (mark-finished state)]
    (let [steps (can-be-done-2 state)
          workers (free-workers state)]
      (cond
        (empty? (:todo state)) (:time state)
        (or (empty? workers) (empty? steps)) (recur (-> state
                                                        (update :time inc)
                                                        mark-finished))
        :else (recur (-> state (assign-tasks workers steps)
                         mark-finished))))))

(defn solve-2
  ([i workers-num base-score] (with-bindings {#'base-cost base-score}
                                (solve-2 i workers-num)))
  ([i workers-num]
   (let [parsed (->> (clojure.string/split i #"\n")
                     (reduce parse {:todo #{}
                                    :done ""
                                    :time 0
                                    :workers (into {} (map (fn [i] [i
                                                                    {:id i :finishes 0}])
                                                           (range workers-num)))}))]
     (loop-2 parsed))))
