(ns aoc2017.day3)

(defn spiral-count
  "Given the index of the spiral (starting at 1) calculate the number of elements in the spiral"
  [n]
  (if (= 1 n) 1 (* 8 (dec n))))

(defn spiral-dist
  "Given the index of a spiral (starting at 1) return a list of the distances from the start of the spiral all the way around the spiral. E.g. the first 3 spiral rows look like:
4 3 2 3 4
3 2 1 2 3
2 1 0 1 2
3 2 1 2 3
4 3 2 3 4
So (spiral-dist 2) returns (1 2 1 2 1 2 1 2)"
  [n]
  (take (spiral-count n)
        (drop 1 (cycle (concat (range (* 2 (dec n)) (- n 2) -1)
                               (range n (* 2 (dec n))))))))

(defn cumulative-count
  "Determine the number of elements in the spiral up to the given 1-based spiral-index"
  [idx]
  (int (Math/pow (inc (* 2 (dec idx))) 2)))

(defn spiral-index
  "Given a number n, determine the 1-based index of the spiral it's in"
  [n]
  (first (drop-while (comp (partial > n) cumulative-count) (map inc (range)))))

(defn spiral-steps
  "Day 3: Compute the number of steps necessary to move from the given index to the center of the spiral"
  [n]
  (let [idx (spiral-index n)
        total (cumulative-count idx)]
    (first (drop (- n (- total (spiral-count idx)) 1) (spiral-dist idx)))))

(def adjacent (juxt dec identity inc))
(def quot-mod (juxt quot mod))

(defn side->mod
  "Given a side from 0-3, compute the modifier in that direction (where 0 is right, 1 is up, 2 is left, 3+ is down"
  [side]
  (first (drop (min 3 side) (filter odd? (range)))))

(defn adj-next-spiral
  "Given a spiral index n, and a side enum (0-4) determine adjacent spiral indices in the side direction"
  [n mod]
  (map (partial + n (spiral-count (spiral-index n)) mod) [-1 0 1]))

(defn adjacent-indices
  "Given a spiral index n, return a set of all adjacent, forward facing indices"
  [n]
  (let [spi-idx (spiral-index n)
        rel-idx (- n (cumulative-count (dec spi-idx)))
        side-length (quot (spiral-count spi-idx) 4)
        [side r] (quot-mod rel-idx side-length)
        precedes-corner? (or (and (< side 3) (= r (dec side-length))) (= side 4))
        is-corner? (and (= r 0) (<= side 3))]
    (filter some?
            (into #{} (concat [(inc n) (when precedes-corner? (+ n 2))]
                              (adj-next-spiral n (side->mod side)) 
                              (when (or (= 1 rel-idx) is-corner?)
                                (adj-next-spiral n (- (side->mod side) 2))))))))

(defn find-target-in-spiral
  "Given a target number, compute spiral sum values until a value is greater than the target"
  [target]
  (loop [dp-vals (zipmap (drop 2 (range)) (repeat 8 1))
         idx 2]
    (let [current-val (get dp-vals idx)]
      (if (< target current-val)
        current-val
        (recur (merge-with + (dissoc dp-vals idx)
                           (zipmap (adjacent-indices idx) (repeat current-val)))
               (inc idx))))))
