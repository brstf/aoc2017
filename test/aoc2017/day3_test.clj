(ns aoc2017.day3-test
  (:require [aoc2017.day3 :refer :all]
            [clojure.test :refer :all]))

(deftest day3-test
  (testing "Spiral Part 1"
    (is (= 0 (spiral-steps 1)))
    (is (= 3 (spiral-steps 12)))
    (is (= 2 (spiral-steps 23)))
    (is (= 31 (spiral-steps 1024)))
    (is (= 438 (spiral-steps 265149))))
  (testing "Spiral Part 2"
    (is (= [1 2 4 5 10 11 23 25 26 54 57 59 122 133 142 147 304 330 351 362 747 806 880 931 957]
         (take 25 (iterate solve-the-problem 1)))
        (= 266330 (find-target-in-spiral 265149)))))

