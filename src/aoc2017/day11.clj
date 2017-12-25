(ns aoc2017.day11)

(defrecord Hex [x y z])

(defonce hex-origin (->Hex 0 0 0))

(defonce ^:private hex-direction
  {"n" (->Hex 0 1 -1)
   "ne" (->Hex 1 0 -1)
   "se" (->Hex 1 -1 0)
   "s" (->Hex 0 -1 1)
   "sw" (->Hex -1 0 1)
   "nw" (->Hex -1 1 0)})

(defn abs [^Integer i] (Math/abs i))

(defn direction->hex
  "Compute a hex given the direction. Coordinates relative to the origin"
  [s]
  (get hex-direction s))

(defn shift-hex
  "Given two hexes, combine them"
  [hex shift-hex]
  (merge-with + hex shift-hex))

(defn move-hex
  "Move from the given hex in the given direction, returning the new hex tile"
  [hex dir]
  (shift-hex hex (direction->hex dir)))

(defn halve [^Integer i] (/ i 2))

(defn hex-distance
  "Given two hexes, compute the distance between them. If only one is given, compute distance from origin"
  ([hex] (hex-distance hex-origin hex))
  ([hex1 hex2]
   (->> (merge-with - hex1 hex2)
        vals
        (map abs)
        (apply +)
        halve)))

(defn hex-path
  "Walk a path of string directions from a given hex. If no start provided, the origin is used. Returns a list of all hexes traversed"
  ([path] (hex-path hex-origin path))
  ([start path] (reductions shift-hex start (map direction->hex path))))

(defn walk-path
  "Same as walk-path, but only returns the final hex reached"
  ([path] (walk-path hex-origin path))
  ([start path] (last (hex-path start path))))
