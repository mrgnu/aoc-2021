(ns aoc-2021.day-3-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-3 :refer :all]))

(deftest unit-test-day-2
  (testing "unit tests day 3"
    (testing "part 1"
      (is (= 198 (->> test-input-3-1 make-report get-power-consumption)))
      )

    (testing "part 2"
      )
    ))

(deftest day-1
  (testing "day 3"
    (testing "part 1"
      (is (= 2003336 (day-3-1)))
      )

    (testing "part 2"
      )
    ))
