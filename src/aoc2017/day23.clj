(ns aoc2017.day23
  (:require [aoc2017.util :refer [read-lines]])
  (:require [aoc2017.day18 :refer [value-of reg-set reg-mul reg-update
                                   step-program terminated? str->kw-or-int]]))

(defn jnz
  "If the value of reg (or the register's value) is not 0 jump val instructions (or the register's value instructions)"
  [reg val state]
  (if (not= 0 (value-of state reg))
    (update state :pc (comp dec (partial + (value-of state val))))
    state))

(defn count-invocations
  "Returns an update function that counts the number of times this update function was invoked, storing this result in the state map at the given key"
  [update-fn count-key]
  (fn [state] (-> (update-fn state)
                 (update count-key (fnil inc 0)))))

(defn parse-fn
  "Parses a single function from the puzzle input. Returns a function that takes a single input: the current program state and returns program state after running the parsed function"
  [fn-map s]
  (let [[f & args] (->> (re-matches #"(\w+) (\S+)(?: (\S+))?" s)
                        (filter (comp not nil?))
                        (drop 1)
                        (map str->kw-or-int))]
    (count-invocations (apply partial (get fn-map f) args) f)))

(def reg-sub (partial reg-update -'))

(def fn-map {:set reg-set :mul reg-mul :jnz jnz :sub reg-sub})

(defn parse-input
  "Parses the puzzle input into a map from pc to function that takes the program state and returns the next program state"
  [s]
  (zipmap (range) (read-lines s (partial parse-fn fn-map))))

(defn run-program
  "Runs the given program until it terminates"
  ([programs] (run-program programs (zipmap [:a :b :c :d :e :f :g :h] (repeat 0))))
  ([programs initial-registers]
   (loop [state {:registers initial-registers :pc 0}]
     (if (terminated? programs state)
       state
       (recur (step-program programs state))))))

(defn prime?
  "Check whether or not n is prime"
  [n]
  (every? (comp pos? (partial mod n)) (range 2 (inc (Math/sqrt n)))))

(defn count-non-primes
  "Counts the non-prime numbers in the range from start->stop stepping by step.
   This is what the assembly program does."
  [start stop step]
  (count (filter false? (map prime? (range start (inc stop) step)))))
