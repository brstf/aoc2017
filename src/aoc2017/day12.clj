(ns aoc2017.day12
  (:require [aoc2017.util :refer [str->int read-lines]]
            [clojure.string :refer [split trim]]))

(def pipe-regex #"(\d+)\s+<->\s([\d\, ]+)")

(defn str->map
  "Given the two strings representing a pipe definition, return a map from the program id to a set of IDs that program is connected to"
  [source-str dests-str]
  {(str->int source-str)
   (into #{} (map str->int (split (trim dests-str) #", ")))})

(defn read-pipe
  "Parse a line of the pipe string input into a map represntation"
  [s]
  (->> (re-matches pipe-regex s)
       (drop 1)
       (apply str->map)))

(defn parse-pipes
  "Parse all pipes from the given text"
  [s]
  (apply merge (read-lines s read-pipe)))

(defn connected-component
  "Compute the connected-component containing the given program using the given set of pipes"
  [pipes program]
  (loop [programs #{program}
         component #{}
         rem-pipes pipes]
    (if-let [p (first programs)]
      (recur (reduce conj (disj programs p) (get rem-pipes p))
             (conj component p) (dissoc rem-pipes p))
      {:component component :remaining rem-pipes})))

(defn connected-components
  "Compute all connected components based on the given pipe map"
  [pipes]
  (loop [components []
         rem-pipes pipes]
    (if (empty? rem-pipes)
      components
      (let [{:keys [component remaining]}
            (connected-component rem-pipes (ffirst rem-pipes))]
        (recur (conj components component) remaining)))))

(defn find-component
  "Find the component containing the given program in the list of components"
  [program components]
  (first (drop-while #(not (contains? % program)) components)))
