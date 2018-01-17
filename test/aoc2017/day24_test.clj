(ns aoc2017.day24-test
  (:require [aoc2017.day24 :refer :all]
            [clojure.test :refer :all]))

(def day24-sample (parse-input (slurp "resources/day24-sample.txt")))
(def day24-input (parse-input (slurp "resources/day24-input.txt")))

(deftest day24-test
  (let [sample-tree (bridge-tree day24-sample)
        input-tree (bridge-tree day24-input)]
    (testing "Part 1: Best Bridge"
      (is (= 31 (best-score (paths sample-tree))))
      (is (= 1695 (best-score (paths input-tree)))))
    (testing "Part 2: Longest Bridge"
      (is (= 19 (longest-bridge (paths sample-tree))))
      (is (= 1673 (longest-bridge (paths input-tree)))))))

