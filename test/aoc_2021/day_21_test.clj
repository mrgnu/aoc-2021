(ns aoc-2021.day-21-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-21 :refer :all]))

(deftest unit-test-day-21
  (testing "unit tests day 21"
    (testing "part 1"
      (is (= 739785
             (->> test-input-21-1
                  read-players
                  (play-game (deterministic-die-seq))
                  compute-game-score
                  )
             ))
      )

    (testing "part 2"
      (is (= {3 1, 4 3, 5 6, 6 7, 7 6, 8 3, 9 1}
             (roll-permutations [1 2 3] 3)))
      )
    ))

(deftest day-21
  (testing "day 21"
    (testing "part 1"
      (is (= 428736 (day-21-1)))
      )

    (testing "part 2"
      ;; NOTE: takes ~2m to run
      (comment
        (is (= 57328067654557N (day-21-2)))
        )
      )
    ))
