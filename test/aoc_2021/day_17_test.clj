(ns aoc-2021.day-17-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-17 :refer :all]))

(deftest unit-test-day-17
  (testing "unit tests day 17"
    (testing "part 1"
      (is (= 45 (->> test-input-17-1 read-target-area find-max-height)))
      )

    (testing "part 2"
      (is (= 112 (->> test-input-17-1 read-target-area find-all-trajectories)))
      )
    ))

(deftest day-17
  (testing "day 17"
    (testing "part 1"
      (is (= 12090 (day-17-1)))
      )

    (testing "part 2"
      (is (= 5059 (day-17-2)))
      )
    ))
