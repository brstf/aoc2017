(ns aoc2017.day15-test
  (:require [aoc2017.day15 :refer :all]
            [clojure.test :refer :all]))

(deftest day15-test
  (testing "Part 1: Lower 16 bit match count"
    (is (= 588 (matching-values 65 8921 40e6)))
    (is (= 609 (matching-values 883 879 40e6))))
  (testing "Part 2: Picky generators"
    (is (= 309 (matching-values 65 (divisible-by? 4) 8921 (divisible-by? 8) 5e6)))
    (is (= 253 (matching-values 883 (divisible-by? 4) 879 (divisible-by? 8) 5e6)))))
