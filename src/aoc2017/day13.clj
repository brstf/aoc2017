(ns aoc2017.day13
  (:require [clojure.string :refer [split]]
            [aoc2017.util :refer [str->int read-lines boomerange]]))

(defn str->firewall-def
  "Convert a string of the form \"{depth}: {size}|\" and returns a map from the depth to the size"
  [s]
  (->> (split s #": ")
       (map str->int)
       (apply (partial assoc {}))))

(defn str->firewall
  "Given an input string, read each line into a depth -> size map, and return a sorted map containing all depth/size pairs sorted by depth"
  [s]
  (into (sorted-map) (apply merge (read-lines s str->firewall-def))))

(defn severity
  "Default severity function for getting caught, depth * size"
  [depth size]
  (* depth size))

(defn scanner-pos
  "Compute the scanner position at the given step"
  [step size]
  (first (drop step (boomerange size))))

(defn caught
  "Return true if you are caught while stepping through a layer with the given size at the given step"
  [step size]
  (= 0 (scanner-pos step size)))

(defn penalty
  "Given a step, depth, size, and optional cost function, return the penalty for moving through the layer at the given depth, 0 if not caught"
  [step depth size]
  (if (caught step size)
    (severity depth size)
    0))

(defn traverse-firewall
  "Move through the firewall with the given starting delay"
  ([firewall] (traverse-firewall firewall 0))
  ([firewall start]
   (->> (map (fn [[d s]] (penalty (+ start d) d s)) firewall)
        (apply +))))

(defn permitted-delay?
  "Given a depth/size pair for a firewall scanner, return a function that given a delay returns true if the given delay is not forbidden"
  [[depth size]]
  (let [forbidden-val (mod (- depth) (* 2 (dec size)))]
    (fn [delay] (not= forbidden-val (mod delay (* 2 (dec size)))))))

(defn permitted-delays
  "Returns a lazy sequence of all possible delay values"
  [firewall]
  (reduce #(filter (permitted-delay? %2) %1) (range) firewall))

(defn optimal-traversal-delay
  "Find the optimal number of picoseconds to delay before moving through the firewall"
  [firewall]
  (first (permitted-delays firewall)))
