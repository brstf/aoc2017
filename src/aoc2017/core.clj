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

(defn str->table [s]
  (->> (clojure.string/split s #"\n")
       (map #(map  str->int (clojure.string/split % #"\s")))))

(defn row-range
  [row]
  (- (apply max row) (apply min row)))

(defn evenly-div
  [row]
  (let [indexed-row (zipmap (range) row)]
    (first (for [[i1 n1] indexed-row [i2 n2] indexed-row
                 :when (and (not= i1 i2) (= 0 (rem n1 n2)))]
             (/ n1 n2)))))

(defn checksum
  [table]
  (reduce + (map row-range table)))

(defn checksum2
  [table]
  (reduce + (map evenly-div table)))
