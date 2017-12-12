(ns aoc2017.util)

(defn str->int [s]
  (Integer/parseInt s))

(defn read-lines
  "Parses a given string into lines, optionally transforming each line by the given function"
  ([s] (read-lines s identity))
  ([s tfn] (map tfn (clojure.string/split s #"\n"))))

(defn str->table [s]
  (read-lines s #(map str->int (clojure.string/split % #"\s"))))
