(ns aoc2017.day2)

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
  "Day 2: Compute the checksum of the given table by sum of all max-min in each row"
  [table]
  (reduce + (map row-range table)))

(defn checksum2
  "Day 2: Compute the checksum instead as the sum of all evenly divided values of each row"
  [table]
  (reduce + (map evenly-div table)))
