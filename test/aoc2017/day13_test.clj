(ns aoc2017.day13-test
  (:require [aoc2017.day13 :refer :all]
            [clojure.test :refer :all]))

(def day13-sample (str->firewall (slurp "test/aoc2017/day13_sample.txt")))
(def day13-input (str->firewall (slurp "test/aoc2017/day13_input.txt")))

(deftest day13-test
  (testing "Part 1: Cost of immediate traversal"
    (is (= 24 (traverse-firewall day13-sample)))
    (is (= 2164 (traverse-firewall day13-input))))
  (testing "Part 2: Avoiding detection"
    (is (= 10 (optimal-traversal-delay day13-sample)))
    (is (= 3861798 (optimal-traversal-delay day13-input)))))

