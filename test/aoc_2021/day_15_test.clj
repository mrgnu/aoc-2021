(ns aoc-2021.day-15-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-15 :refer :all]))

(deftest unit-test-day-15
  (testing "unit tests day 15"
    (testing "part 1"
      (is (= 40 (->> test-input-15-1 read-risk-level-map get-total-risk)))
      )

    (testing "part 2"
      )
    ))

(deftest day-15
  (testing "day 15"
    (testing "part 1"
      (is (= 366 (day-15-1)))
      )

    (testing "part 2"
      )
    ))
