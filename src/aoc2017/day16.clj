(ns aoc2017.day16
  (:require [aoc2017.util :refer [index->char-keyword str->int]]))

(defn initial-positions
  "Given the number of programs participating in a dance, return a vector of symbols representing those programs, e.g. (initial-positions 5) => [:a :b :c :d :e]"
  [n]
  (vec (map index->char-keyword (range n))))

(defn exchange
  "Exchange the values at the two given indices p1 and p2 in the given vector v"
  [p1 p2 v]
  (assoc v p1 (get v p2) p2 (get v p1)))

(defn partner
  "Swap the places of the programs with the given names (as keywords) in the given vector"
  [n1 n2 v]
  (exchange (.indexOf v n1) (.indexOf v n2) v))

(defn spin
  "Spin all positions in the vector by shifting all elements to the right and putting in any values that \"fall off\" at the front of the vector"
  [n v]
  (let [c (count v)]
    (vec (take c (drop (- c n) (cycle v))))))

(defn display-str
  "Given a vector of symbols, return a string that can be used for display"
  [v]
  (apply str (map name v)))

(defn parse-spin
  "Parse a spin function out of the given string, nil if the string isn't of the form sN"
  [s]
  (when-let [match (re-matches #"s(\d+)" s)]
    (partial spin (str->int (second match)))))

(defn parse-exchange
  "Parse an exchange function out of the given string, nil if the string isn't of the form xA/B"
  [s]
  (when-let [match (re-matches #"x(\d+)/(\d+)" s)]
    (partial exchange (str->int (nth match 1)) (str->int (nth match 2)))))

(defn parse-partner
  "Parse a partner function out of the given string, nil if the string isn't of the form pA/B"
  [s]
  (when-let [match (re-matches #"p(.+?)/(.+?)" s)]
    (partial partner (keyword (nth match 1)) (keyword (nth match 2)))))

(defn str->fn
  "Given a string that should represent a dance step function and return the appropriate function"
  [s]
  (or (parse-spin s) (parse-exchange s) (parse-partner s)))

(defn parse-input
  "Parses a comma separated list of dance step inputs into corresponding functions"
  [s]
  (->> (clojure.string/split s #",")
       (map str->fn)))

(defn shut-up-and-dance
  "Apply the given set of dance-steps to the given initial positions"
  [dance-steps positions]
  ((apply comp (reverse dance-steps)) positions))

(defn repeat-dance
  "Repeats the same set of dance steps n times"
  [dance-steps initial-pos n]
  (let [steps (iterate (partial shut-up-and-dance dance-steps) initial-pos)
        period (inc (count (take-while (partial not= initial-pos) (drop 1 steps))))]
    (nth steps (rem n period))))
