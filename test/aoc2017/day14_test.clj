(ns aoc2017.day14-test
  (:require [aoc2017.day14 :refer :all]
            [clojure.test :refer :all]))

(def day14-sample (str->disk "flqrgnkx"))
(def day14-input (str->disk "vbqugkhl"))

(deftest day14-test
  (testing "Part 1: Count used spaces"
    (is (= 8108 (count-used day14-sample)))
    (is (= 8148 (count-used day14-input))))
  (testing "Part 2: Count connected components"
    (is (= 1242 (count (connected-components (disk->usage-map day14-sample)))))
    (is (= 1180 (count (connected-components (disk->usage-map day14-input)))))))

