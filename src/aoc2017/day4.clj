(ns aoc2017.day4)

(defn passphrase-check-fn [tx-fn]
  (fn [passphrase]
    (apply distinct? (map tx-fn (clojure.string/split passphrase #"\s+")))))

(def passphrase-distinct? (passphrase-check-fn identity))
(def no-anagrams? (passphrase-check-fn frequencies))
