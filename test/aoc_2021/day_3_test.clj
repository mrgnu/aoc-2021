(ns aoc-2021.day-3-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-3 :refer :all]))

(deftest unit-test-day-3
  (testing "unit tests day 3"
    (testing "part 1"
      (is (= 198 (->> test-input-3-1 make-report get-power-consumption)))
      )

    (testing "part 2"
      (is (= 230 (->> test-input-3-1 make-report get-life-support-rating)))
      )
    ))

(deftest day-3
  (testing "day 3"
    (testing "part 1"
      (is (= 2003336 (day-3-1)))
      )

    (testing "part 2"
      (is (= 1877139 (day-3-2)))
      )
    ))
