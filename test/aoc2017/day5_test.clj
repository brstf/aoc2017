(ns aoc2017.day5-test
  (:require [aoc2017.day5 :refer :all]
            [clojure.test :refer :all]
            [aoc2017.util :refer [str->int read-lines]]))

(def day5-input
  (read-lines (slurp "test/aoc2017/day5_input.txt") str->int))

(deftest day5-test
  (testing "Day 5: Part 1 Step count"
    (is (= 5 (step-count [0 3 0 1 -3])))
    (is (= 372139 (step-count day5-input))))
  (testing "Day 5: Part 2 Strange steps"
    (is (= 10 (step-count-strange [0 3 0 1 -3])))
    (is (= 29629538 (step-count-strange day5-input)))))
