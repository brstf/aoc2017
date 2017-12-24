(ns aoc2017.day9)

(defonce escape-regex #"!.")
(defonce garbage-regex #"<.*?>")

(defn filter-escapes [s]
  (clojure.string/replace s escape-regex ""))

(defn filter-garbage [s]
  (clojure.string/replace s garbage-regex ""))

(defn filter-group [s]
  (apply str (filter (partial contains? #{\{ \}}) s)))

(defn sanitize
  "Given a string input, clean up all escape characters/garbage, then create a nested vector representing the stream's data."
  [s]
  (-> (filter-group (filter-garbage (filter-escapes s)))
      (clojure.string/replace #"\{" "[")
      (clojure.string/replace #"\}" "]")
      read-string
      eval))

(defn score
  "Compute the score of a day9 stream. Accepts either a nested vector of inputs or a string of inputs which will be converted to a vector of inputs first before scoring"
  ([s] (score (if (string? s) (sanitize s) s) 1))
  ([stream n]
   (if (vector? stream)
     (+ n (apply + (map #(score % (inc n)) stream)))
     n)))

(defn count-garbage
  "Given a string input, count the number of non-escaped garbage characters that appear in the input"
  [s]
  (apply + (map (comp dec dec count) (re-seq garbage-regex (filter-escapes s)))))
