(ns aoc-2021.day-11-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-11 :refer :all]))

(deftest unit-test-day-11
  (testing "unit tests day 11"
    (testing "part 1"
      (is (= 1656 (->> test-input-11-1 read-octopus-grid (count-flashes 100))))
      )

    (testing "part 2"
      (is (= 195 (->> test-input-11-1 read-octopus-grid find-full-flash)))
      )
    ))

(deftest day-11
  (testing "day 11"
    (testing "part 1"
      (is (= 1588 (day-11-1)))
      )

    (testing "part 2"
      (is (= 517 (day-11-2)))
      )
    ))
