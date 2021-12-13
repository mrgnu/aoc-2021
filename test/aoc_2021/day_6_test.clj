(ns aoc-2021.day-6-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-6 :refer :all]))

(deftest unit-test-day-6
  (testing "unit tests day 6"
    (testing "part 1"
      (is (= 26N (->> test-input-6-1 read-fish (count-fish-lifelines 18))))
      (is (= 5934N (->> test-input-6-1 read-fish (count-fish-lifelines 80))))

      (testing "count-fish-lifeline"
        (is (= 1N (count-fish-lifeline 0 0)))

        (is (= 2N (count-fish-lifeline 1 0)))
        (is (= 2N (count-fish-lifeline 2 0)))
        (is (= 2N (count-fish-lifeline 7 0)))

        (is (= 3N (count-fish-lifeline 8 0)))
        (is (= 3N (count-fish-lifeline 9 0)))

        (is (= 4N (count-fish-lifeline 10 0)))
        (is (= 4N (count-fish-lifeline 14 0)))

        (is (= 5N (count-fish-lifeline 15 0)))
        (is (= 5N (count-fish-lifeline 16 0)))

        (is (= 7N (count-fish-lifeline 17 0)))
        )
    )

  (testing "part 2"
    )
  ))

(deftest day-6
  (testing "day 6"
    (testing "part 1"
      (is (= 365131N (day-6-1)))
      )

    (testing "part 2"
      )
    ))
