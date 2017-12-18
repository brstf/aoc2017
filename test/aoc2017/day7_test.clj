(ns aoc2017.day7-test
  (:require [aoc2017.day7 :refer :all]
            [clojure.test :refer :all]))

(def sample-tree (parse-input (slurp "test/aoc2017/day7_sample.txt")))
(def day7-tree (parse-input (slurp "test/aoc2017/day7_input.txt")))

(deftest day7-test
  (testing "Part 1: Find root"
    (is (= "tknk" (find-root sample-tree)))
    (is (= "vtzay" (find-root day7-tree))))
  (testing "Part 2: Find balance tree"
    (is (= 60 (:weight (first (filter-rebalanced (balance-tree sample-tree))))))
    (is (= 910 (:weight (first (filter-rebalanced (balance-tree day7-tree))))))))
