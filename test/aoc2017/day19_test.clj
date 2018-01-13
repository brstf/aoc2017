(ns aoc2017.day19-test
  (:require [aoc2017.day19 :refer :all]
            [clojure.test :refer :all]))

(def sample (parse-input (slurp "resources/day19-sample.txt")))
(def input (parse-input (slurp "resources/day19-input.txt")))

(deftest day19-test
  (testing "Part 1: Path string"
    (is (= "ABCDEF" (walk-path sample)))
    (is (= "RYLONKEWB" (walk-path input))))
  (testing "Part 2: Path length"
    (is (= 38 (count-path sample)))
    (is (= 16016 (count-path input)))))

