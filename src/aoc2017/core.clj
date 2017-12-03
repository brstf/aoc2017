(ns aoc2017.core)

(defn str->list [s]
  (map #(str->int (str %)) s))

(defn str->int [s]
  (Integer/parseInt s))

(def captcha-xf (comp (filter (partial apply =)) (map first)))

(defn reverse-captcha
  "Day 1: Given a list of integers, return the sum of all digits followed by a duplicate"
  [captcha]
  (->> (cycle captcha)
       (take (inc (count captcha)))
       (partition 2 1)
       (transduce captcha-xf +)))

(defn reverse-captcha-half-step
  "Day 1, part 2: Reverse captcha with half-step comparisons"
  [captcha]
  (->> (interleave captcha (drop (/ (count captcha) 2) (cycle captcha)))
       cycle
       (partition 2)
       (take (count captcha))
       (transduce captcha-xf +)))
