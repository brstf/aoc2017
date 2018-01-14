(ns aoc2017.day22
  (:require [aoc2017.util :refer [read-lines]]))

(defn parse-line
  "Parses a line into the column numbers that are already infected"
  [s]
  (->> (zipmap (range) s)
       (filter (comp (partial = \#) second))
       (map first)
       (map (partial + (- (quot (count s) 2))))
       (#(zipmap % (repeat "#")))))

(defn parse-input
  "Parses the input puzzle into a map from row num to a set of columns that start infected"
  [s]
  (let [rows (read-lines s parse-line)]
    (zipmap (map (partial + (- (quot (count rows) 2))) (range)) rows)))

(defn get-val
  "Gets the value of the given position in the program grid"
  [state [x y]]
  (get-in state [y x]))

(defn set-val
  "Sets the val at the given position in the program grid"
  [state [x y] val]
  (assoc-in state [y x] val))

(def directions
  "List of directions in clockwise order"
  [[-1 0] [0 -1] [1 0] [0 1]])

(def left
  "Map from direction [x y] to the direction reached by turning left"
  (zipmap directions (take 4 (drop 3 (cycle directions)))))

(def right
  "Map from direction [x y] to the direction reached by turning right"
  (zipmap directions (take 4 (drop 1 (cycle directions)))))

(defn rvrs
  "Computes the reverse from direction [x y] to the reverse direction, [-x -y]"
  [dir]
  (vec (map - dir)))

(defn infect
  "Infect the grid at the given position and increment the total infected count"
  [state [x y]]
  (-> (assoc-in state [y x] "#")
      (update :inf-count inc)))

(defn clean
  "Clean the value at the given position"
  [state [x y]]
  (update state y dissoc x))

(defn next-dir
  "Given the current value and direction, returns the next direction the virus travels in"
  [val dir]
  (case val
    "#" (right dir)
    "W" dir
    "F" (rvrs dir)
    (left dir)))

(defn update-val
  "Update function that moves from clean -> infected -> clean"
  [state pos]
  (case (get-val state pos)
    "#" (clean state pos)
    (infect state pos)))

(defn update-weakened
  "Update function that moves from clean -> weakend -> infected -> flagged -> clean"
  [state pos]
  (case (get-val state pos)
    "W" (infect state pos)
    "#" (set-val state pos "F")
    "F" (clean state pos)
    (set-val state pos "W")))

(defn initialize-state
  "Add initial position, direction, and total infection count"
  [state]
  (assoc state :dir [0 -1] :pos [0 0] :inf-count 0))

(defn burst
  "Using the given functions to determine the next direction and update the current value, update the current state according to the virus's next burst of activity"
  [dir-fn update-fn {:keys [dir pos inf-count] :as state}]
  (let [new-dir (dir-fn (get-val state pos) dir)]
    (-> (update-fn state pos)
        (assoc :pos (vec (map + new-dir pos)))
        (assoc :dir new-dir))))

(def burst1
  "Burst function for part 1"
  (partial burst next-dir update-val))

(def burst2
  "Burst function for part 2, adding weakened / flagged tiles"
  (partial burst next-dir update-weakened))

(defn repeat-burst
  "Repeat the viruses burst of activity for n iterations, returns the state after the last iteration"
  [burst-fn state n]
  (last (take (inc n) (iterate burst-fn (initialize-state state)))))
