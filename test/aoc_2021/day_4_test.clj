(ns aoc-2021.day-4-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-4 :refer :all]))

(deftest unit-test-day-4
  (testing "unit tests day 4"
    (testing "part 1"
      (is (= 3 (count (:boards (read-bingo-game test-input-4-1)))))
      (is (= 4512 (play-bingo-game (read-bingo-game test-input-4-1))))
      )

    (testing "part 2"
      )
    ))

(deftest day-4
  (testing "day 4"
    (testing "part 1"
      (is (= 8580 (->> (input-4-1) read-bingo-game play-bingo-game)))
      )

    (testing "part 2"
      )
    ))
