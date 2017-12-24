(ns aoc2017.day9-test
  (:require [aoc2017.day9 :refer :all]
            [clojure.test :refer :all]))

(def day9-input (slurp "test/aoc2017/day9_input.txt"))

(deftest day9-test
  (testing "Part 1"
    (is (= 1 (score "{}")))
    (is (= 6 (score "{{{}}}")))
    (is (= 5 (score "{{}, {}}")))
    (is (= 16 (score "{{{},{},{{}}}}")))
    (is (= 1 (score "{<a>,<a>,<a>,<a>}")))
    (is (= 9 (score "{{<ab>},{<ab>},{<ab>},{<ab>}}")))
    (is (= 9 (score "{{<!!>},{<!!>},{<!!>},{<!!>}}")))
    (is (= 3 (score "{{<a!>},{<a!>},{<a!>},{<ab>}}")))
    (is (= 7616 (score day9-input))))
  (testing "Part 2: Count garbage"
    (is (= 0 (count-garbage "<>")))
    (is (= 17 (count-garbage "<random characters>")))
    (is (= 3 (count-garbage "<<<<>")))
    (is (= 2 (count-garbage "<{!>}>")))
    (is (= 0 (count-garbage "<!!>")))
    (is (= 0 (count-garbage "<!!!>>")))
    (is (= 10 (count-garbage "<{o\"i!a,<{i<a>")))
    (is (= 3838 (count-garbage day9-input)))))
