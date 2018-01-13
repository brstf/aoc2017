(ns aoc2017.day20
  (:require [aoc2017.util :refer [str->int read-lines]]))

(defrecord Vector [x y z])
(defrecord Particle [pos vel acc])

(defn add-vectors
  "Adds all given vectors together into a single vector"
  [& vs]
  (apply merge-with +' vs))

(defn parse-vector
  "Parses an input vector <#,#,#> into a Vector record"
  [s]
  (->> (re-matches #"<(-?\d+),(-?\d+),(-?\d+)>" s)
       (drop 1)
       (map str->int)
       (apply ->Vector)))

(defn parse-particle
  "Parses an input particle p=<> v=<> a=<> into a Particle record"
  [s]
  (->> (re-matches #"p=(<.*?>), v=(<.*?>), a=(<.*?>)" s)
       (drop 1)
       (map parse-vector)
       (apply ->Particle)))

(defn parse-input
  "Parses an input string of particles into a vector of Particle records"
  [s]
  (into [] (read-lines s parse-particle)))

(defn magnitude
  "Computes the magnitude of a given vector (sqrt (+ x*x y*y z*z))"
  [^Vector v]
  (->> (map val v)
       (map #(*' % %))
       (apply +')
       Math/sqrt))

(defn min-vector
  "Finds the particle ID with minimum magnitude of the given key (:pos, :vec, or :acc)"
  [key particles]
  (->> (map (comp magnitude key) particles)
       (zipmap (range))
       (apply min-key val)
       first))

(def min-acceleration "Finds the particle with minimum acceleration"
  (partial min-vector :acc))

(defn update-particle
  "Updates a particle by updating its position and velocity"
  [^Particle p]
  (let [new-vel (add-vectors (:vel p) (:acc p))]
    (->Particle (add-vectors (:pos p) new-vel) new-vel (:acc p))))

(defn destroy-collisions
  "Removes any particles that have the same position"
  [particles]
  (->> (sort-by (comp vec vals :pos) particles)
       (partition-by (comp vec vals :pos))
       (filter (comp (partial = 1) count))
       flatten))

(defn particle-states
  "Returns a lazy sequence of particle states taken by sequentially updating all particles simultaneously. If n is supplied return the first n elements"
  ([particles]
   (lazy-seq (cons particles (->> (map update-particle particles)
                                  destroy-collisions
                                  particle-states))))
  ([particles n] (take n (particle-states particles))))
