(ns aoc2017.day11-test
  (:require [aoc2017.day11 :refer :all]
            [clojure.test :refer :all]
            [clojure.string :refer [split trim]]))

(def day11-path (-> (trim (slurp "test/aoc2017/day11_input.txt"))
                    (split #",")))

(deftest day11-test
  (testing "Part 1: Shortest path"
    (is (= 687 (hex-distance (walk-path day11-path)))))
  (testing "Part 2: Farthest distance"
    (is (= 1483 (apply max (map hex-distance (hex-path day11-path)))))))

