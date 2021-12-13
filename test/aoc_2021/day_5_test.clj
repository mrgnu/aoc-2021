(ns aoc-2021.day-5-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-5 :refer :all]))

(deftest unit-test-day-5
  (testing "unit tests day 5"
    (testing "part 1"
      (is (= 5 (->> test-input-5-1 read-lines build-vent-map (count-vents 2))))
      )

    (testing "part 2"
      )
    ))

(deftest day-5
  (testing "day 5"
    (testing "part 1"
      (is (= 6461 (day-5-1)))
      )

    (testing "part 2"
      )
    ))
