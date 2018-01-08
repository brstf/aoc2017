(ns aoc2017.day17-test
  (:require [aoc2017.day17 :refer :all]
            [clojure.test :refer :all]))

(deftest day17-test
  (testing "Part 1: 2017 short-circuit"
    (is (= 638 (short-circuit 2017 (spinerate 3 2017))))
    (is (= 777 (short-circuit 2017 (spinerate 376 2017)))))
  (testing "Part 2: 50 million steps"
    (is (= 39289581 (last-position 376 50e6 1)))))
