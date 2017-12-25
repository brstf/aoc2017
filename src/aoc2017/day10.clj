(ns aoc2017.day10)

(defn twist
  "Twists the input sequence based on the given position in length"
  [seq pos length]
  (->> (take (count seq) (drop pos (cycle seq)))
       (split-at length)
       (#(concat (reverse (first %)) (second %)))
       cycle
       (drop (- (count seq) pos))
       (take (count seq))))

(defn knot-hash
  "Compute the knot-hash of a given length using the supplied lengths. If rounds is specified, knot-hash will run that number of times, otherwise it will only be run once"
  ([loop-length lengths] (knot-hash loop-length lengths 1))
  ([loop-length lengths rounds]
   (loop [seq (range loop-length)
          pos 0
          skip 0
          rem-lengths lengths
          rem-rounds (dec rounds)]
     (if-let [length (first rem-lengths)]
       (recur (twist seq pos length) (mod (+ pos skip length) loop-length)
              (inc skip) (rest rem-lengths) rem-rounds)
       (if (> rem-rounds 0)
         (recur seq pos skip lengths (dec rem-rounds))
         seq)))))

(defn str->lengths
  "Convert an arbitrary string into a sequence of lengths to use as the input to knot-hash"
  [s]
  (concat (map int s) [17 31 73 47 23]))

(defn dense-hash
  "Given a sparse-hash of 256 ints, condense into a dense-hash of 16 ints"
  [sparse-hash]
  (->> (partition 16 sparse-hash)
       (map (partial reduce bit-xor))))

(defn hash->str
  "Convert a sequence of ints representing a hash into a hexadecimal string"
  [hash]
  (.toLowerCase (apply str (map (partial format "%02X") hash))))

(defn str->dense-hash
  "Given a string, use the knot-hash algorithm to produce a dense-hash"
  [s]
  (->> (knot-hash 256 (str->lengths s) 64)
       dense-hash
       hash->str))
