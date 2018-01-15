(ns aoc2017.day18
  (:require [aoc2017.util :refer [read-lines str->int]]))

(defn value-of
  "If reg is a register, retrieve the register's value from the program state, otherwise return the given value as is"
  [state reg]
  (if (keyword? reg) (get-in state [:registers reg] 0) reg))

(defn reg-set
  "Set register reg to the given value (either a register or integer)"
  [reg val state]
  (assoc-in state [:registers reg] (value-of state val)))

(defn reg-update
  "Update the value of the given value by the update-fn and value of val"
  [update-fn reg val state]
  (assoc-in state [:registers reg] (update-fn (value-of state reg)
                                              (value-of state val))))

(def reg-add (partial reg-update +'))
(def reg-mul (partial reg-update *'))
(def reg-mod (partial reg-update mod))

(defn reg-sound
  "Emit a sound of the given frequency. If val is a register, emits a sound of the registers frequency"
  [val state]
  (assoc state :sound (value-of state val)))

(defn reg-recover
  "Recovers the frequency of the last sound played if the given value is not 0"
  [val state]
  (if (not= 0 (value-of state val))
    (assoc state :recovered (:sound state))
    state))

(defn reg-send
  "Send the value of the given register out of this program"
  [val state]
  (-> (assoc state :sent (value-of state val))
      (update :total-sent inc)))

(defn reg-receive
  "If a value has been sent to this program, receive it into the current register. Otherwise decrement the program counter so that this instruction will be executed again"
  [reg state]
  (if-let [rcvd (first (:received state))]
    (-> (assoc-in state [:registers reg] rcvd)
        (update :received (comp vec rest)))
    (update state :pc dec)))

(defn jgz
  "If the value of reg (or the registers value) is greater than 0 jump val instructions (or the register's value instructions)"
  [reg val state]
  (if (> (value-of state reg) 0)
    (update state :pc (comp dec (partial + (value-of state val))))
    state))

(defn terminated?
  "Checks if the program is terminated due to the program counter being out of bounds of the current program"
  [programs state]
  (not (<= 0 (:pc state) (dec (count programs)))))

(defn step-program
  "Steps the given program forward by executing the current instruction"
  [programs state]
  (-> ((get programs (:pc state)) state)
      (update :pc inc)))

(defn str->kw-or-int
  "Converts a string to an integer if it's an integer, otherwise return a keyword with a name of the string"
  [s]
  (if (re-matches #"-?\d+" s) (str->int s) (keyword s)))

(defn parse-fn
  "Parses a single function from the puzzle input. Returns a function that takes a single input: the current program state and returns program state after running the parsed function"
  [fn-map s]
  (let [[f & args] (->> (re-matches #"(\w+) (\S+)(?: (\S+))?" s)
                        (filter (comp not nil?))
                        (drop 1)
                        (map str->kw-or-int))]
    (apply partial (get fn-map f) args)))

(defn parse-input
  "Parses the puzzle input into a map from pc to function that takes the program state and returns the next program state"
  [fn-map s]
  (zipmap (range) (read-lines s (partial parse-fn fn-map))))

(def common-fns {:set reg-set :add reg-add :mul reg-mul :mod reg-mod :jgz jgz})
(def part1-fn-map (merge common-fns {:snd reg-sound :rcv reg-recover}))
(def part2-fn-map (merge common-fns {:snd reg-send :rcv reg-receive}))
(def parse-part1 (partial parse-input part1-fn-map))
(def parse-part2 (partial parse-input part2-fn-map))

(defn first-recovered
  "Runs the program until a single value is recovered, return that value"
  [programs]
  (->> (iterate (partial step-program programs) {:pc 0 :registers {}})
       (drop-while (comp not (partial :recovered)))
       first
       :recovered))

(defn sent->received
  "Transfer register values from prog-b to prog-a, return prog-a"
  [prog-a prog-b]
  (if (:sent prog-b)
    (update (merge {:received []} prog-a) :received (comp vec conj) (:sent prog-b))
    prog-a))

(defn duet
  "Runs the duet function until both programs terminate. Returns the number of times program with pid 1 sent a value"
  [programs]
  (loop [prog0 {:pc 0 :total-sent 0 :pid 0 :registers {:p 0}}
         prog1 {:pc 0 :total-sent 0 :pid 1 :registers {:p 1}}]
    (let [next0 (step-program programs prog0)
          next1 (step-program programs prog1)]
      (if (and (or (terminated? programs next0) (apply = (map :pc [prog0 next0])))
               (or (terminated? programs next1) (apply = (map :pc [prog1 next1]))))
        (:total-sent next1)
        (recur (dissoc (sent->received next0 next1) :sent)
               (dissoc (sent->received next1 next0) :sent))))))
