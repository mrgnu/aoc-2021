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
      (let [winners (deplete-bingo-game (read-bingo-game test-input-4-1))
            {:keys [draw winners]} (last winners)]
        (is (= 1 (count winners)))
        (is (= 1924 (compute-score draw (first winners))))
        )
      )
    ))

(deftest day-4
  (testing "day 4"
    (testing "part 1"
      (is (= 8580 (->> (input-4-1) read-bingo-game play-bingo-game)))
      )

    (testing "part 2"
      (is (= 9576 (day-4-2)))
      )
    ))
