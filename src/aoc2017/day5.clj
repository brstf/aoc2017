(ns aoc2017.day5)

(defn call-stack
  "Returns the sequence of indices executed by executing the program with steps represented by the given sequence"
  [step-seq mod-fn]
  (loop [index 0
         step-map (transient (zipmap (range) step-seq))
         stack (transient [])]
    (if-let [step-val (get step-map index)] 
      (recur (+ index step-val) (assoc! step-map index ((mod-fn step-val) step-val))
             (conj! stack index))
      (persistent! stack))))

(defn step-count
  "Given a step sequence, return the number of steps executed by the \"CPU\" until the program terminates"
  [sq]
  (count (call-stack sq (constantly inc))))

(defn strange-step-update
  [offset]
  (if (>= offset 3) dec inc))

(defn step-count-strange
  [sq]
  (count (call-stack sq strange-step-update)))
