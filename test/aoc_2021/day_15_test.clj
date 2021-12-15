(ns aoc-2021.day-15-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-15 :refer :all]
            [aoc-2021.utils :as utils]))

(deftest unit-test-day-15
  (testing "unit tests day 15"
    (testing "part 1"
      (is (= 40 (->> test-input-15-1 read-risk-level-map get-total-risk)))
      )

    (testing "part 2"
      (testing "expand-map"
        (let [expected (->> "resources/day_15_2_test.txt"
                            utils/per-line-input
                            read-risk-level-map)
              actual (->> test-input-15-1
                          read-risk-level-map
                          expand-map)]
          (is (= expected actual)))
        )
      (is (= 315 (->> test-input-15-1 read-risk-level-map expand-map get-total-risk)))
      )
    ))

(deftest day-15
  (testing "day 15"
    (testing "part 1"
      (is (= 366 (day-15-1)))
      )

    (testing "part 2"
      ;; NOTE: slow
      (comment
        (is (= 2829 (day-15-2)))
        )
      )
    ))
