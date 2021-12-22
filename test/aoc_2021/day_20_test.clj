(ns aoc-2021.day-20-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-20 :refer :all]))

(deftest unit-test-day-20
  (testing "unit tests day 20"
    (testing "part 1"
      (is (= 35
             (->> (test-input-20-1)
                  read-image
                  ;; enhancement-map[0] == .
                  (enhance-n-times 2 (repeat \.))
                  :pixels
                  vals
                  (filter (partial = \#))
                  count
                  )
             ))
      )

    (testing "part 2"
      (is (= 3351
             (->> (test-input-20-1)
                  read-image
                  ;; enhancement-map[0] == .
                  (enhance-n-times 50 (repeat \.))
                  :pixels
                  vals
                  (filter (partial = \#))
                  count
                  )
             ))
      )
    ))

(deftest day-20
  (testing "day 20"
    (testing "part 1"
      ;; first wrong guess
      (is (< (day-20-1) 5776))
      ;; second wrong guess
      (is (< (day-20-1) 5761))
      ;; third wrong guess
      (is (not= (day-20-1) 5746))
      ;; correct guess
      (is (= (day-20-1) 5682))
      )

    (testing "part 2"
      )
    ))
