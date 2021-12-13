(ns aoc-2021.day-1-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-1 :refer :all]))

(deftest unit-test-day-1
  (testing "unit tests day 1"
    (testing "part 1"
      (testing "count-depth-increases"
        (is (= 7 (count-depth-increases test-input-1-1)))
        ))
    )
  )

(deftest day-1
  (testing "day 1"
    (testing "part 1"
      (is (= 1446 (day-1-1))))
    )
  )
