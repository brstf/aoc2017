(ns aoc2017.day20-test
  (:require [aoc2017.day20 :refer :all]
            [clojure.test :refer :all]))

(def day20-sample (parse-input (slurp "resources/day20-sample.txt")))
(def day20-input (parse-input (slurp "resources/day20-input.txt")))

(deftest day20-test
  (testing "Part 1: Closest particle"
    (is (= 258 (min-acceleration day20-input))))
  (testing "Part 2: Colliding particles"
    (is (= 1 (last (map count (particle-states day20-sample 3)))))
    (is (= 707 (last (map count (particle-states day20-input 1000)))))))
