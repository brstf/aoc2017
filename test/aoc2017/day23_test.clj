(ns aoc2017.day23-test
  (:require [aoc2017.day23 :refer :all]
            [clojure.test :refer :all]))

(def day23-input (parse-input (slurp "resources/day23-input.txt")))

(deftest day23-test
  (testing "Part 1: Run some more assembly"
    (is (= 9409 (:mul (run-program day23-input)))))
  (testing "Part 2: Assembly deciphering"
    (is (= 913 (count-non-primes 109900 126900 17)))))

