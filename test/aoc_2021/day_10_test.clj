(ns aoc-2021.day-10-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-10 :refer :all]))

(deftest unit-test-day-10
  (testing "unit tests day 10"
    (testing "part 1"
      (is (= 26397 (->> test-input-10-1 parse-program program-error-score)))
      )

    (testing "part 2"
      (is (= 288957
             (->> test-input-10-1 parse-program program-completion-score)))

      )
    ))

(deftest day-10
  (testing "day 10"
    (testing "part 1"
      (is (= 216297 (day-10-1)))
      )

    (testing "part 2"
      (is (= 2165057169 (day-10-2)))
      )
    ))
