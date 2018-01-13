(ns aoc2017.day21
  (:require [aoc2017.util :refer [read-lines]]))

(def initial-pattern "Starting pattern for exploding"
  [[\. \# \.] [\. \. \#] [\# \# \#]])

(defn parse-pattern
  "Parses a single pattern into a vector of vector pattern"
  [s]
  (into [] (map vec (clojure.string/split s #"/"))))

(defn parse-mapping
  "Parses a single line into a map from left hand pattern to right hand pattern"
  [s]
  (->> (re-matches #"(\S+) => (\S+)" s)
       (drop 1)
       (map parse-pattern)
       (apply hash-map)))

(defn parse-input
  "Parses the input string into a map from pattern to larger pattern"
  [s]
  (apply merge (read-lines s parse-mapping)))

(defn rotate
  "Rotates a pattern clockwise"
  [pattern]
  (apply map (comp reverse vector) pattern))

(defn flip
  "Flips a pattern horizontally"
  [pattern]
  (map reverse pattern))

(defn subdivide
  "Subdivides a pattern into n*n patterns, returns a list of lists of split patterns"
  [pattern n]
  (->> (map vec (partition n pattern))
       (map (partial map (comp (partial map vec) (partial partition n))))
       (map (partial apply map vector))))

(defn join-patterns
  "Joins a set of subdivided patterns back into a single large pattern"
  [patterns]
  (->> (apply map concat patterns)
       (apply map concat)))

(defn pattern->str
  "Converts a pattern to a string for more sane display purposes"
  [pattern]
  (apply str (flatten (interleave pattern (repeat \newline)))))

(defn pattern-match
  "Given a pattern, find it's corresponding explosion in the given pattern map. Apply combinations of flip/rotates to find the match"
  [pattern-map pattern]
  (->> ((juxt identity (partial map flip)) (take 4 (iterate rotate pattern)))
       (apply concat)
       (map (partial get pattern-map))
       (filter some?)
       first))

(defn explode-pattern
  "Explode the given pattern one iteration"
  ([pattern-map] (explode-pattern pattern-map initial-pattern))
  ([pattern-map pattern]
   (let [d (if (= 0 (mod (count pattern) 2)) 2 3)
         divided (subdivide pattern d)
         exploded (map (partial map (partial pattern-match pattern-map)) divided)]
     (join-patterns exploded))))

(defn explode-n
  "Explode the fractal pattern for n iterations, return the last iteration"
  [pattern-map n]
  (->> (iterate (partial explode-pattern pattern-map) initial-pattern)
       (take (inc n))
       last))

(defn count-on
  "Count the number of tiles that are 'on' in the given pattern"
  [pattern]
  (count (filter (partial = \#) (flatten pattern))))
