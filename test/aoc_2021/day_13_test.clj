(ns aoc-2021.day-13-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-13 :refer :all]))

(deftest unit-test-day-13
  (testing "unit tests day 13"
    (testing "part 1"
      (is (= 17
             (->> test-input-13-1
                  read-manual
                  perform-fold
                  :dots
                  count
                  )))
      )

    (testing "part 2"
      )
    ))

(deftest day-13
  (testing "day 13"
    (testing "part 1"
      (is (= 850 (day-13-1)))
      )

    (testing "part 2"
      )
    ))
