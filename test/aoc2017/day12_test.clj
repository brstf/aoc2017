(ns aoc2017.day12-test
  (:require [aoc2017.day12 :refer :all]
            [clojure.test :refer :all]))

(def day12-sample-pipes (parse-pipes (slurp "test/aoc2017/day12_sample.txt")))
(def day12-input-pipes (parse-pipes (slurp "test/aoc2017/day12_input.txt")))

(deftest day12-test
  (testing "Part 1: Connected component containing 0"
    (is (= 6 (count (find-component 0 (connected-components day12-sample-pipes)))))
    (is (= 134 (count (find-component 0 (connected-components day12-input-pipes))))))
  (testing "Part 2: Number of components"
    (is (= 2 (count (connected-components day12-sample-pipes))))
    (is (= 193 (count (connected-components day12-input-pipe))))))

