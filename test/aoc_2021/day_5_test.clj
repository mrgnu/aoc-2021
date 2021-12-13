(ns aoc-2021.day-5-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-5 :refer :all]))

(deftest unit-test-day-5
  (testing "unit tests day 5"
    (testing "part 1"
      ;;; NOTE: diags were not considered in first part
      (comment (is (= 5 (->> test-input-5-1 read-lines build-vent-map (count-vents 2)))))
      )

    (testing "part 2"
      (is (= 12 (->> test-input-5-1 read-lines build-vent-map (count-vents 2))))
      )
    ))

(deftest day-5
  (testing "day 5"
    (testing "part 1"
      ;;; NOTE: diags were not considered in first part
      (comment (is (= 6461 (day-5-1))))
      )

    (testing "part 2"
      (is (= (day-5-2) 18065))
      )
    ))
