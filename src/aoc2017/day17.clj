(ns aoc2017.day17)

(defn spin
  "Step the spinlock forward one step in it's algorithm. Increments the current position by n, inserts the next element after the next position"
  [n {:keys [position buffer]}]
  (let [newpos (inc (mod (+ position n) (count buffer)))
        [buf1 buf2] (split-at newpos buffer)]
    {:position newpos :buffer (concat buf1 [(count buffer)] buf2)}))

(defn spinerate
  "Steps through n iterations of the spin-lock algorithm, moving the position by steps on each iteration"
  [steps n]
  (loop [state {:position 0 :buffer [0]}
         remaining n]
    (if (= 0 remaining) state (recur (spin steps state) (dec remaining)))))

(defn short-circuit
  "Find the short-circuit value of a buffer, where the short-circuit value is the value in the buffer *after* circuit-val."
  [circuit-val {:keys [buffer]}]
  (second (drop-while (partial not= circuit-val) (cycle buffer))))

(defn last-position
  "Iterate through n iterations of the spinlock algorithm, stepping \"steps\" times on each iteration, and return the element at pos in the buffer after n iterations"
  [steps n pos]
  (loop [step 1 current-pos 1 pos-value 1]
    (if (= step n)
      (if (= 1 current-pos) step pos-value)
      (recur (inc step) (inc (mod (+ current-pos steps) (inc step)))
             (if (= 1 current-pos) step pos-value)))))
