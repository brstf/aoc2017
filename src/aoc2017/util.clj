(ns aoc2017.util)

(defn str->int [s]
  (Integer/parseInt s))

(defn str->table [s]
  (->> (clojure.string/split s #"\n")
       (map #(map  str->int (clojure.string/split % #"\s")))))
