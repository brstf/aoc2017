(ns aoc2017.util)

(defn str->int [s]
  (Integer/parseInt s))

(defn read-lines
  "Parses a given string into lines, optionally transforming each line by the given function"
  ([s] (read-lines s identity))
  ([s tfn] (map tfn (clojure.string/split s #"\n"))))

(defn str->table [s]
  (read-lines s #(map str->int (clojure.string/split % #"\s"))))

(defn boomerange [n]
  (cycle (concat (range 0 (dec n)) (range (dec n) 0 -1))))

(def index->char-keyword
  "Given an index from 0-25, return a keyword of the letter of that index, e.g. 0 = :a, 1 = :b, etc."
  (comp keyword str char (partial + 97)))
