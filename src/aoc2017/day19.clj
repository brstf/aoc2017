(ns aoc2017.day19
  (:require [aoc2017.util :refer [read-lines]]))

(defn parse-input
  "Parse the input string into a nested vector for use"
  [s]
  (into (vector) (read-lines s vec)))

(defn col
  "Retrieve the column at index c"
  [tubes c]
  (map #(nth % c) tubes))

(def row "Retrieve the row at the given index" nth)

(defn starting-position
  "Given the nested vector representing the tubes, find the starting position of the path"
  [tubes]
  [(.indexOf (row tubes 0) \|) 0])

(def vertical? "Checks whether or not a direction is vertical" #{:+y :-y})
(def horizontal? "Checks whether or not a direction is horizontal" #{:+x :-x})
(def positive? "Checks whether or not a direction is positive" #{:+y :+x})
(def negative? "Checks whether or not a direction is negative" #{:-y :-x})

(defn negate-dir
  "Given a direction, return the opposite direction"
  [dir]
  (keyword (apply str (replace {\- \+ \+ \-} (name dir)))))

(defn get-slice
  "Get a slice starting at the given position moving in the given direction until the edge of the input is reached"
  [tubes [x y] dir]
  (if (vertical? dir) (col tubes x) (row tubes y)))

(defn get-char
  "Gets the character from the input at the given position"
  [tubes [x y]]
  (get-in tubes [y x]))

(defn follow
  "Starting at position [x y] follow the current direction until a '+' character is encountered. (dir is #{:+x :-x :+y :-y})"
  [tubes [x y] dir]
  (let [i (if (vertical? dir) y x)]
    (->> (get-slice tubes [x y] dir)
         ((if (negative? dir)
            (comp reverse (partial take (inc i)))
            (partial drop i)))
         (apply str)
         (re-matches #"(\+?[^\+\s]+).*")
         (second))))

(defn update-pos
  "Given a position and the direction of the path followed, return the new position after moving n characters in that direction"
  [pos dir n]
  (update pos (if (horizontal? dir) 0 1) + (if (negative? dir) (- n) n)))

(defn update-dir
  "Determine the new direction for the path at the given position after moving from the given direction"
  [tubes pos from-dir]
  (->> (map (partial map + pos) [[-1 0] [0 1] [1 0] [0 -1]])
       (map (partial get-char tubes))
       (zipmap [:-x :+y :+x :-y])
       (filter (comp (partial not= (negate-dir from-dir)) key))
       (filter (comp (partial re-matches #"\S") str second))
       (ffirst)))

(defn step-follow
  "Follow the path in the given direction until the path turns"
  [tubes {:keys [pos dir path]}]
  (let [segment (follow tubes pos dir)
        next-pos (update-pos pos dir (count segment))]
    {:pos next-pos
     :dir (update-dir tubes next-pos dir)
     :path (str path segment)}))

(defn path->str
  "Given a path, return the non-path "
  [path]
  (clojure.string/replace path #"[^A-Z]" ""))

(defn follow-path
  "Follows the path from the given input, returning the string representing all elements in the order traversed"
  [tubes]
  (loop [state {:pos (starting-position tubes) :dir :+y :path ""}]
    (let [{:keys [dir pos path] :as next-state} (step-follow tubes state)]
      (if dir (recur next-state) path))))

(defn walk-path
  "Follows the path in tubes and returns the alphabetic characters in order traversed"
  [tubes]
  (path->str (follow-path tubes)))

(defn count-path
  "Follows the path in tubes and returns the number of steps taken"
  [tubes]
  (count (follow-path tubes)))
