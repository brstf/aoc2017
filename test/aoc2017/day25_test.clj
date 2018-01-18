(ns aoc2017.day25-test
  (:require [aoc2017.day25 :refer :all]
            [clojure.test :refer :all]))

(def day25-input (parse-input (slurp "resources/day25-input.txt")))

(deftest day25-test
  (testing "Day 25: Turing Machines!"
    (is (= 2725 (count (run-steps day25-input))))))

