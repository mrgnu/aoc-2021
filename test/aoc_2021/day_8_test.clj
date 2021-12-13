(ns aoc-2021.day-8-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-8 :refer :all]))

(deftest unit-test-day-8
  (testing "unit tests day 8"
    (testing "part 1"
      (is (= 26 (->> test-input-8-1 parse-segment-specs get-known-output-digit-count)))
      )

    (testing "part 2"
      (is (= 61229 (->> test-input-8-1 parse-segment-specs (map map-outputs) (apply +))))
      )
    ))

(deftest day-8
  (testing "day 8"
    (testing "part 1"
      (is (= 421 (day-8-1)))
      )

    (testing "part 2"
      (is (= 986163 (day-8-2)))
      )
    ))
