(ns aoc2017.day22-test
  (:require [aoc2017.day22 :refer :all]
            [clojure.test :refer :all]))

(def day22-input (parse-input (slurp "resources/day22-input.txt")))

(deftest day22-test
  (testing "Part 1: The Game of Flu"
    (is (= 5176 (:inf-count (repeat-burst burst1 day22-input 1e4)))))
  (testing "Part 2: More statuses"
    (is (= 2512017 (:inf-count (repeat-burst burst2 day22-input 10e6))))))

