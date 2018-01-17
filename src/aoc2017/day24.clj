(ns aoc2017.day24
  (:require [aoc2017.util :refer [read-lines str->int]]))

(defn parse-component
  "Given a string of the form #/#, parse out a 2-element vector representing the magnetized bridge component"
  [s]
  (mapv str->int (re-seq #"-?\d+" s)))

(defn parse-input
  "Parses the given string into a list of bridge pieces"
  [s]
  (read-lines s parse-component))

(defn valid-components
  "Computes a list of valid components that can have an input that matches the given input-value"
  [in-val components]
  (filter (partial some #{in-val}) components))

(defn score-bridge
  "Computes the score of a bridge"
  [components]
  (apply + (flatten components)))

(defn rest-components
  "Removes the given component from the list of components"
  [component components]
  (seq (remove #{component} components)))

(defn bridge-tree
  "Returns a tree of bridge pieces such that each child of a bridge is a valid next segment so following a path from the root to a leaf returns a valid bridge"
  ([components] (bridge-tree components [0 0]))
  ([components [prev-in prev-out :as prev-component]]
   (if-let [valid (valid-components prev-out components)]
     (cons prev-component
           (mapv (fn [[in out :as match]]
                   (bridge-tree (rest-components match components)
                                (if (= in prev-out) match [out in]))) valid))
     [prev-component])))

(defn paths
  "Given a bridge-tree, return all paths in the tree"
  [[root & children]]
  (if children
    (->> (mapcat paths children)
         (mapv (comp vec (partial cons root))))
    [[root]]))

(defn best-score
  "Given a list of bridges, return the bridge with the best score"
  [bridges]
  (apply max (map score-bridge bridges)))

(defn longest-bridge
  "Given a list of bridges, return the score of the best longest bridge"
  [bridges]
  (->> bridges
       (filter (comp (partial = (apply max (map count bridges))) count))
       best-score))
