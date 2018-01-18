(ns aoc2017.day25
  (:require [aoc2017.util :refer [str->int]]))

(def rule-regex "Regex for parsing a state's rule"
  #"[^:]*?the value ([01])[^:]*?slot to the (.*?)\.[^:]*?state ([A-Z])[^:]*?")

(defn dir->int
  "Converts a direction left/right to an amount to shift the tape position by"
  [s]
  (case s "left" -1 "right" 1))

(defn parse-rule
  "Parses a state's rule into a map representing the rule"
  [s]
  (->> (re-matches rule-regex s)
       (drop 1)
       (map vector)
       (map apply [str->int dir->int keyword])
       (zipmap [:write :shift :move])))

(defn parse-state
  "Parses a state into a map from cursor value to the rule for that value"
  [s]
  (->> (re-matches #"In state ([A-Z]):[^:]*?:([^:]+)If the current[^:]*?:([^:]+)" s)
       (drop 1)
       (map vector)
       (map apply [keyword parse-rule parse-rule])
       ((fn [[state & rules]] {state (zipmap [0 1] rules)}))))

(defn parse-initial
  "Parses the initial state \"header\" string to parse the initial state the machine is in and how many steps to take"
  [s]
  {:state (keyword (second (re-find #"state ([A-Z])" s)))
   :steps (str->int (re-find #"\d+" s))})

(defn str->machine
  "Converts a header string and sequence of state strings into a machine map"
  [initial-str state-strs]
  {:initial (parse-initial (first initial-str))
   :states (into {} (map parse-state state-strs))})

(defn parse-input
  "Parses an input file into a machine map"
  [s]
  (->> (clojure.string/split s #"\n\n")
       (split-at 1)
       (apply str->machine)))

(defn update-state
  "Given the current state and map of machine states, return the state after moving one iteration"
  [{:keys [cursor state tape] :as current-state} states-map]
  (let [tape-val (if (tape cursor) 1 0)
        {:keys [write shift move]} (get-in states-map [state tape-val])]
    {:cursor (+ cursor shift)
     :state move
     :tape (case write 0 (disj tape cursor) 1 (conj tape cursor))}))

(defn run-steps
  "Run a machine for the number of steps in the machine state. Returns the set of tape indices that are 1 after the program terminates"
  [{:keys [initial states] :as machine}]
  (loop [remaining (:steps initial)
         state {:cursor 0 :state (:state initial) :tape #{}}]
    (if (= 0 remaining)
      (:tape state)
      (recur (dec remaining) (update-state state states)))))
