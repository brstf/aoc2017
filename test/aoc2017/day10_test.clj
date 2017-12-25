(ns aoc2017.day10-test
  (:require [aoc2017.day10 :refer :all]
            [clojure.test :refer :all]
            [aoc2017.util :refer [str->int]]
            [clojure.string :refer [split trim]]))

(def day10-input-str (trim (slurp "test/aoc2017/day10_input.txt")))
(def day10-input (->> (split day10-input-str  #",")
                      (map trim)
                      (map str->int)))

(deftest day10-test
  (testing "Part 1: Knot hash"
    (is (= [3 4 2 1 0] (knot-hash 5 [3 4 1 5])))
    (is (= 11375 (apply * (take 2 (knot-hash 256 day10-input))))))
  (testing "Part 2: Dense hash"
    (is (= "a2582a3a0e66e6e86e3812dcb672a272" (str->dense-hash "")))
    (is (= "33efeb34ea91902bb2f59c9920caa6cd" (str->dense-hash "AoC 2017")))
    (is (= "3efbe78a8d82f29979031a4aa0b16a9d" (str->dense-hash "1,2,3")))
    (is (= "63960835bcdc130f0b66d7ff4f6a5a8e" (str->dense-hash "1,2,4")))
    (is (= "e0387e2ad112b7c2ef344e44885fe4d8" (str->dense-hash day10-input-str)))))



