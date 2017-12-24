(ns aoc2017.day8-test
  (:require [aoc2017.day8 :refer :all]
            [clojure.test :refer :all]))

(def day8-sample (read-input (slurp "test/aoc2017/day8_sample.txt")))
(def day8-input (read-input (slurp "test/aoc2017/day8_input.txt")))

(deftest day8-test
  (testing "Part 1: Largest register end value"
    (is (= 1 (apply max (vals (run-program day8-sample)))))
    (is (= 5849 (apply max (vals (run-program day8-input))))))
  (testing "Part 2: Largest register throughout"
    (is (= 10 (largest-register (run-program-history day8-sample))))
    (is (= 6702 (largest-register (run-program-history day8-input))))))

