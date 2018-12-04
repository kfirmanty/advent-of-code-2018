(ns advent-of-code-2018.day4
  (:require [instaparse.core :as insta]
            [advent-of-code-2018.common :as common]))

(def input-parser (insta/parser
                   "
<S>=date <ws> (guard-init | asleep | wakes-up)
guard-init= <'Guard #'> number <' begins shift'>
asleep='falls asleep'
wakes-up='wakes up'
<date>= <'['> <year> <'-'> month <'-'> day <ws> hour <':'> minute <']'>
year=number
month=number
day=number
hour=number
minute=number 
number=#'\\d+'
ws=#'\\s+'"))

(defn log-date [{:keys [month day hour minute]}]
  [month day hour minute])

(defn deep-compare [[[v1 v2] & rest]]
  (cond (< v1 v2) -1
        (> v1 v2) 1
        :else (if (empty? rest)
                0
                (recur rest))))

(defn compare-logs [v1 v2]
  (deep-compare (map vector (log-date v1) (log-date v2))))

(defn consume [event state]
  (cond (:guard-init event)
        (assoc state :current-guard (:guard-init event))
        (:asleep event)
        (assoc state :current-asleep (:minute event))
        (:wakes-up event)
        (update state (:current-guard state)
                update :sleep-overs conj {:from (:current-asleep state) :to (:minute event)})))

(defn most-asleep [guards]
  (->> guards
       (map #(assoc % :total-asleep
                     (reduce (fn [s {:keys [from to]}] (+ s (- to from))) 0 (:sleep-overs %))))
       (sort-by :total-asleep)
       last))

(defn find-overlapping [{:keys [guard-id sleep-overs]}]
  (let [ranges (->> sleep-overs
                    (map #(range (:from %) (:to %)))
                    flatten)
        [minute freq] (->> ranges frequencies (sort-by second) last)]
    [freq guard-id minute]))

(defn consume-events [events]
  (loop [event (first events) events (rest events) state {}]
    (if (nil? event)
      (dissoc state :current-guard :current-asleep)
      (recur (first events) (rest events) (consume event state)))))

(defn add-id [[id val]]
  (assoc val :guard-id id))

(defn parse-input [i]
  (->> (common/parse-input i input-parser)
       (sort compare-logs)
       consume-events
       (map add-id)))

(defn calculate-answer [_ id m]
  (* id m))

(defn solve-1 [i]
  (->> i
       parse-input
       most-asleep
       find-overlapping
       (apply calculate-answer)))

(defn solve-2 [i]
  (->> i
       parse-input
       (map find-overlapping)
       (sort-by first)
       last
       (apply calculate-answer)))
