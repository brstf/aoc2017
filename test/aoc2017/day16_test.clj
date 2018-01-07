(ns aoc2017.day16-test
  (:require [aoc2017.day16 :refer :all]
            [clojure.test :refer :all]))

(def sample-input (parse-input "s1,x3/4,pe/b"))
(def day16-input (->> (.trim (slurp "test/aoc2017/day16_input.txt"))
                      parse-input))

(deftest day16-test
  (testing "Part 1: Dancing programs"
    (is (= "baedc"
           (display-str (shut-up-and-dance sample-input (initial-positions 5)))))
    (is (= "giadhmkpcnbfjelo"
           (display-str (shut-up-and-dance day16-input (initial-positions 16))))))
  (testing "Part 2: One. Billion. Dances."
    (is (= "njfgilbkcoemhpad"
           (display-str (repeat-dance day16-input (initial-positions 16) 1e9))))))
