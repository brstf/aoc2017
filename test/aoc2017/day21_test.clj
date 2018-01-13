(ns aoc2017.day21-test
  (:require [aoc2017.day21 :refer :all]
            [clojure.test :refer :all]))

(def day21-input (parse-input (slurp "resources/day21-input.txt")))

(deftest day21-test
  (testing "Part 1: Fractal explosion"
    (is (= 155 (count-on (explode-n day21-input 5)))))
  (testing "Part 2: More iterations"
    (is (= 2449665 (count-on (explode-n day21-input 18))))))

