(ns aoc2017.day8
  (:require [aoc2017.util :refer [read-lines str->int]]))

;; Group 1: register to modify
;; Group 2: function to use to modify register
;; Group 3: amount to modify
;; Group 4: register for the condition
;; Group 5: Condition to test >,<,>=,<=,==,!=
;; Group 6: Amount to compare register value to
(def instruction-regex
  #"(\S+?)\s+?(dec|inc)\s+?(-?\d+?)\s+?if\s+?(\S+)\s+?(>|<|!=|==|<=|>=)\s+?(-?\d+)")

(defrecord Instruction
    ;; reg - register to modify
    ;; mod-fn function that takes in the current register value and returns the new value
    ;; cond-fn function that takes a map representing the state of all registers and returns true if the register should be modified
    [reg mod-fn cond-fn])

(defn regex->reg
  "Given the result of the instruction-regex, extract the register for the instruction"
  [regex-result]
  (keyword (first regex-result)))

(defn str->fn [s]
  (case s "inc" + "dec" -))

(defn regex->modfn
  "Given the result of the instruction-regex, extract the mod function for the instruction"
  [regex-result]
  (let [modfn (str->fn (second regex-result))
        modby (str->int (nth regex-result 2))]
    (fn [val] (modfn val modby))))

(defn str->cond [s]
  (eval (read-string (if (= s "!=") "not=" s))))

(defn make-cond-fn
  "Given the three necessary values to create a condition function, create the condition function that takes in the current register map, and returns true if the condition is met, false otherwise"
  [register cond-fn comp-val]
  (fn [reg-map] (cond-fn (get reg-map register 0) comp-val)))

(defn regex->condfn
  "Given the result of the instruction-regex, extract the condition function for the instruction"
  [regex-result]
  (->> (map #(%1 %2) [keyword str->cond str->int] (drop 3 regex-result))
       (apply make-cond-fn)))

(defn line->instruction [line]
  (->> (drop 1 (re-matches instruction-regex line))
       ((juxt regex->reg regex->modfn regex->condfn))
       (apply ->Instruction)))

(defn read-input [s]
  (read-lines s line->instruction))

(defn update-registers
  "Given an instruction and a register map, update the registers based on the instruction's mod function if the condition is met"
  [reg-map instr]
  (if ((:cond-fn instr) reg-map)
    (update reg-map (:reg instr) (fnil (:mod-fn instr) 0))
    reg-map))

(defn run-program [instructions]
  (reduce update-registers {} instructions))

(defn run-program-history
  "Same as run-program, but keeps the entire history of the program as well"
  [instructions]
  (reductions update-registers {} instructions))

(defn largest-register
  "Given a program's run history, find the largest register value"
  [history]
  (->> (filter (comp not empty?) history)
       (map (comp (partial apply max) vals))
       (apply max)))
