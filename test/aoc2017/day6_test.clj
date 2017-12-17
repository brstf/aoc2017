(ns aoc2017.day6-test
  (:require [aoc2017.day6 :refer :all]
            [clojure.test :refer :all]
            [aoc2017.util :refer [str->int]]))

(def day6-input
  (map str->int (clojure.string/split (slurp "test/aoc2017/day6_input.txt") #"\s")))

(deftest day6-test
  (testing "Day 6: Part 1 unique memory configuration count"
    (is (= 5 (unique-seqs [0 2 7 0])))
    (is (= 3156 (unique-seqs day6-input))))
  (testing "Day 6: Part 2 loop length"
    (is (= 4 (loop-length [0 2 7 0])))
    (is (= 1610 (loop-length day6-input)))))

