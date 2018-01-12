(ns aoc2017.day18-test
  (:require [aoc2017.day18 :refer :all]
            [clojure.test :refer :all]))

(def day18-part1-sample (parse-part1 (slurp "test/aoc2017/day18_sample.txt")))
(def day18-part1-input (parse-part1 (slurp "test/aoc2017/day18_input.txt")))
(def day18-part2-sample (parse-part2 (slurp "test/aoc2017/day18_sample.txt")))
(def day18-part2-input (parse-part2 (slurp "test/aoc2017/day18_input.txt")))

(deftest day18-test
  (testing "Day 18: Part 1 Recovered sound"
    (is (= 4 (first-recovered day18-part1-sample)))
    (is (= 9423 (first-recovered day18-part1-input))))
  (testing "Day 18: Part 2 Duet"
    (is (= 1 (duet day18-part2-sample)))
    (is (= 7620 (duet day18-part2-input)))))

