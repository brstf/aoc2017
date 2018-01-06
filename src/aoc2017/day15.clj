(ns aoc2017.day15)

(defn generate
  "Creates a generator function that when given a value in the sequence will return the next value in the sequence."
  [factor]
  (fn [prev] (mod (* factor prev) 2147483647)))

(def gen-a (generate 16807))
(def gen-b (generate 48271))

(defn divisible-by? [n]
  (fn [x] (= 0 (mod x n))))

(defn lowest-16-bits [x]
  (bit-and x 0xFFFF))

(defn lowest-16-bits-match?
  [x1 x2]
  (= (lowest-16-bits x1) (lowest-16-bits x2)))

(defn matching-values
  "Given two generator functions and initial values, return how many pairs of numbers in the sequence match in the last 16 bits out of the first n pairs of numbers"
  ([a0 b0 n] (matching-values a0 identity b0 identity n))
  ([a0 pred-a b0 pred-b n]
   (let [gen-a (filter pred-a (drop 1 (iterate gen-a a0)))
         gen-b (filter pred-b (drop 1 (iterate gen-b b0)))]
     (->> (map lowest-16-bits-match? gen-a gen-b)
          (take n)
          (filter true?)
          count))))
