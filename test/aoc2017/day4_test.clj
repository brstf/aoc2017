(ns aoc2017.day4-test
  (:require  [clojure.test :refer :all]
             [aoc2017.day4 :refer :all]))

(def test-phrases
  (clojure.string/split (slurp "test/aoc2017/day4_input.txt") #"\n"))

(deftest day4-test
  (testing "Day 4: Part 1 Passphrases"
    (is (= true (passphrase-distinct? "aa bb cc dd ee")))
    (is (= false (passphrase-distinct? "aa bb cc dd aa")))
    (is (= true (passphrase-distinct? "aa bb cc dd aaa")))
    (is (= 325 (count (filter passphrase-distinct? test-phrases)))))
  (testing "Day 4: Part 2 Passphrase no anagrams"
    (is (= true (no-anagrams? "abcde fghij")))
    (is (= false (no-anagrams? "abcde xyz ecdab")))
    (is (= true (no-anagrams? "a ab abc abd abf abj")))
    (is (= true (no-anagrams? "iiii oiii ooii oooi oooo")))
    (is (= false (no-anagrams? "oiii ioii iioi iiio")))
    (is (= 119 (count (filter no-anagrams? test-phrases))))))


