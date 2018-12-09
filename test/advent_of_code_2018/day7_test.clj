(ns advent-of-code-2018.day7-test
  (:require [advent-of-code-2018.day7 :as d]
            [clojure.test :refer :all]))

(deftest should-pass-description
  (is (= 15 (d/solve-2 "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin." 2 0))))

(deftest should-pass-part-2
  (is (= 1053 (d/solve-2 "Step X must be finished before step C can begin.
Step C must be finished before step G can begin.
Step F must be finished before step G can begin.
Step U must be finished before step Y can begin.
Step O must be finished before step S can begin.
Step D must be finished before step N can begin.
Step M must be finished before step H can begin.
Step J must be finished before step Q can begin.
Step G must be finished before step R can begin.
Step I must be finished before step N can begin.
Step R must be finished before step K can begin.
Step A must be finished before step Z can begin.
Step Y must be finished before step L can begin.
Step H must be finished before step P can begin.
Step K must be finished before step S can begin.
Step Z must be finished before step P can begin.
Step T must be finished before step S can begin.
Step N must be finished before step P can begin.
Step E must be finished before step S can begin.
Step S must be finished before step W can begin.
Step W must be finished before step V can begin.
Step L must be finished before step V can begin.
Step P must be finished before step B can begin.
Step Q must be finished before step V can begin.
Step B must be finished before step V can begin.
Step P must be finished before step Q can begin.
Step S must be finished before step V can begin.
Step C must be finished before step Q can begin.
Step I must be finished before step H can begin.
Step A must be finished before step E can begin.
Step H must be finished before step Q can begin.
Step G must be finished before step V can begin.
Step N must be finished before step L can begin.
Step R must be finished before step Q can begin.
Step W must be finished before step L can begin.
Step X must be finished before step L can begin.
Step X must be finished before step J can begin.
Step W must be finished before step P can begin.
Step U must be finished before step B can begin.
Step P must be finished before step V can begin.
Step O must be finished before step P can begin.
Step W must be finished before step Q can begin.
Step S must be finished before step Q can begin.
Step U must be finished before step Z can begin.
Step Z must be finished before step T can begin.
Step M must be finished before step T can begin.
Step A must be finished before step P can begin.
Step Z must be finished before step B can begin.
Step N must be finished before step S can begin.
Step H must be finished before step N can begin.
Step J must be finished before step E can begin.
Step M must be finished before step J can begin.
Step R must be finished before step A can begin.
Step A must be finished before step Y can begin.
Step F must be finished before step V can begin.
Step L must be finished before step P can begin.
Step K must be finished before step L can begin.
Step F must be finished before step P can begin.
Step G must be finished before step L can begin.
Step I must be finished before step Q can begin.
Step C must be finished before step L can begin.
Step I must be finished before step Y can begin.
Step G must be finished before step B can begin.
Step H must be finished before step L can begin.
Step X must be finished before step U can begin.
Step I must be finished before step K can begin.
Step R must be finished before step N can begin.
Step I must be finished before step L can begin.
Step M must be finished before step I can begin.
Step K must be finished before step V can begin.
Step G must be finished before step E can begin.
Step F must be finished before step B can begin.
Step O must be finished before step Y can begin.
Step Y must be finished before step Q can begin.
Step F must be finished before step K can begin.
Step N must be finished before step W can begin.
Step O must be finished before step R can begin.
Step N must be finished before step E can begin.
Step M must be finished before step V can begin.
Step H must be finished before step T can begin.
Step Y must be finished before step T can begin.
Step F must be finished before step J can begin.
Step F must be finished before step O can begin.
Step W must be finished before step B can begin.
Step T must be finished before step E can begin.
Step T must be finished before step P can begin.
Step F must be finished before step M can begin.
Step U must be finished before step I can begin.
Step H must be finished before step S can begin.
Step S must be finished before step P can begin.
Step T must be finished before step W can begin.
Step A must be finished before step N can begin.
Step O must be finished before step N can begin.
Step L must be finished before step B can begin.
Step U must be finished before step K can begin.
Step Z must be finished before step W can begin.
Step X must be finished before step D can begin.
Step Z must be finished before step L can begin.
Step I must be finished before step T can begin.
Step O must be finished before step W can begin.
Step I must be finished before step B can begin." 5))))