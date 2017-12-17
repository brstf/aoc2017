(ns aoc2017.day6)

(defn update-indices
  [sq idx]
  (frequencies (take (nth sq idx) (drop (inc idx) (cycle (range (count sq)))))))

(defn sq->map [sq] (zipmap (range) sq))
(defn map->sq [m] (into [] (map (partial get m) (range (count m)))))

(defn select-max [sq]
  (->> (partition 2 (interleave (range) sq))
       (drop-while (comp (partial not= (apply max sq)) second))
       ffirst))

(defn rebalance-memory
  ([sq] (rebalance-memory sq select-max update-indices))
  ([sq select-fn update-fn]
   (let [update-idx (select-fn sq)]
     (map->sq (merge-with + (assoc (sq->map sq) update-idx 0)
                          (update-fn sq update-idx))))))

(defn rebalance-seq
  "Returns a lazy sequence of each memory rebalance performed"
  [sq]
  (rebalance-memory sq))

(defn memory-seq
  "Returns a sequence of the configurations of memory banks starting at the given sequence and proceeding up until (and including) a memory configuration is seen that occurred previously"
  [sq]
  (->> (iterate rebalance-memory sq)
       (map repeat)
       ;; conjing into a set and a list is for improved performance, can just use
       ;; distinct? on the list, but that involves a linear time check for each
       ;; iteration
       (map (partial zipmap [:list :set]))
       (reductions (partial merge-with conj) {:list [] :set #{}})
       (drop 1)
       (drop-while (fn [{:keys [list set]}] (= (count list) (count set))))
       first
       :list))

(defn unique-seqs
  "Returns the number of unique memory sequences before a duplicate is encountered"
  [sq]
  (dec (count (memory-seq sq))))

(defn loop-length
  "Returns the number of unique memory sequences in the first loop during memory rebalance"
  [sq]
  (let [mem (memory-seq sq)]
    (dec (count (drop-while (partial not= (last mem)) mem)))))
