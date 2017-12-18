(ns aoc2017.day7
  (:require [clojure.string :refer [split]])
  (:require [aoc2017.util :refer [str->int read-lines]]))

(defrecord Program [name weight children])

(def ^:private prog-regex #"(.*?)\s\((\d*?)\)(?:\s*?->\s*?((?:\S+,?\s?)+))?")

(defn parse-children [children]
  (if children (split children #",\s*") []))

(defn line->program
  "Given a string line of the form \"name (weight)[ -> list, of, children]\" return a program record of the data contained in the string"
  [line]
  (->> (re-matches prog-regex line)
       (drop 1)
       (map #(%1 %2) [identity str->int parse-children])
       (apply ->Program)))
 
(defn parse-input [s]
  (->> (read-lines s line->program)
       (reduce #(assoc %1 (:name %2) %2) {})))

(defn find-root
  "Given a program map of program names to Program records, find the name of the root program"
  [programs]
  (->> (map :children (vals programs))
       (reduce #(apply disj %1 %2) (into #{} (keys programs)))
       first))

(defn balanced?
  "Checks if a Program with the given name is balanced"
  [programs prog-name]
  (let [program (get programs prog-name)]
    (>= 1 (count (into #{} (map #(get-in programs [% :supporting-weight])
                                (:children program)))))))

(defn adjust-weight
  "Adjusts the weight of a child program map, return a Program map containing only the fixed child"
  [child diff]
  {(:name child)
   (assoc child
          :weight (- (:weight child) diff)
          :rebalanced? true
          :supporting-weight (- (:supporting-weight child) diff))})

(defn fix-subtree [programs program]
  (if (not (balanced? (merge {(:name program) program} programs) (:name program)))
    (let [children (map (partial get programs) (:children program))
          fixed-child (->> (map (partial assoc {}) (map :supporting-weight children)
                                (map (comp vector :name) children))
                           (apply merge-with concat)
                           (sort-by (comp count second))
                           ((juxt (comp first second first)
                                  (comp (partial apply -) (partial map first))))
                           (#(adjust-weight (get programs (first %)) (second %))))]
      (merge programs fixed-child))
    programs))

(defn supporting-weight
  [programs program]
  (reduce + (:weight program)
          (map :supporting-weight
               (map (partial get programs) (:children program)))))

(defn balance-subtree
  "Given a program map and a program name, balance the subtree starting with that program. Add supporting weights and fix subtrees if necessary to balance the tree"
  [programs prog-name]
  (let [program (get programs prog-name)
        children-weights (apply merge (map (partial balance-subtree programs)
                                           (:children program)))
        fixed-children (fix-subtree children-weights program)]
    (merge {prog-name (assoc program :supporting-weight
                             (supporting-weight fixed-children program))}
           fixed-children)))

(defn balance-tree
  "Given a program map, return a map from program name to the Program record with the amount of weight supported by the program added at the key :supporting-weight. Adjusts the weight of any necessary programs to balance the tree, adding a :rebalanced? true flag if the Program had to be rebalanced"
  [programs]
  (balance-subtree programs (find-root programs)))

(defn filter-rebalanced
  [programs]
  (filter :rebalanced? (vals programs)))
