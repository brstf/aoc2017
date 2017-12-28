(ns aoc2017.day14
  (:require [aoc2017.day10 :refer [str->dense-hash]]
            [aoc2017.util :refer [str->int]]))

(defn hex->binary
  "Given a hex character, return a sequence of 4 bits representing the characters value"
  [c]
  (->> (Integer/parseInt (str c) 16)
       (Integer/toBinaryString)
       (map (comp str->int str))
       ((fn [binary] (concat (repeat (- 4 (count binary)) 0) binary)))))

(defn str->disk
  "Given a string, compute 128 knot-hashes from the string, and convert those hashes to binary. Returns a list of 128 sequences of 128 ints, 0 or 1"
  [s]
  (->> (map str (repeat s) (repeat "-") (range 128))
       (pmap str->dense-hash)
       (map (partial mapcat hex->binary))
       (doall)))

(defn disk->display-str
  "Given a disk, print out the #. formatting used in the example, # for used . for unused."
  [disk]
  (->> (map (comp clojure.string/join (partial map {0 "." 1 "#"})) disk)
       (clojure.string/join "\n")))

(defn count-used
  "Count the number of used squares in the given disk"
  [disk]
  (apply + (map (comp count (partial filter (partial = 1))) disk)))

(defn only-used
  "Filter all rows for the indices of used squares"
  [row]
  (->> (map-indexed vector row)
       (filter (comp (partial = 1) second))
       (map first)
       (into #{})))

(defn disk->usage-map
  "Convert a disk into a \"usage map\". Returns a set of all [row col] pairs of used squares"
  [disk]
  (->> (map only-used disk)
       (map-indexed #(partition 2 (interleave (repeat %1) %2)))
       (reduce concat [])
       (into #{})))

(defn valid-index? [index]
  (<= 0 index 127))

(defn adjacencies
  "Return a list of [row col] pairs that are adjacent to the given [row col] pair"
  [row col]
  (->> (map (comp (partial into []) (partial map +))
            (repeat [row col]) [[0 1] [1 0] [-1 0] [0 -1]])
       (filter (partial every? valid-index?))))

(defn used? [usage-map [row col]]
  (contains? usage-map [row col]))

(defn used-adjacencies
  "Given a [row col] pair and a usage map, return all adjacent used squares"
  [[row col] usage-map]
  (filter (partial used? usage-map) (adjacencies row col)))

(defn connected-component
  "Given the usage-map and a [row col] pair, find the connected component using DFS"
  [usage-map [row col]]
  (loop [visited #{}
         to-visit #{[row col]}]
    (if-let [square (first to-visit)]
      (recur (conj visited square)
             (->> (used-adjacencies square usage-map)
                  (filter (comp not (partial contains? visited)))
                  (reduce conj (disj to-visit square))))
      visited)))

(defn connected-components
  "Given a usage-map, find all connected-components in the map"
  [usage-map]
  (loop [components []
         used-squares usage-map]
    (if-let [square (first used-squares)]
      (let [component (connected-component used-squares square)]
        (recur (conj components component) (reduce disj used-squares component)))
      components)))
