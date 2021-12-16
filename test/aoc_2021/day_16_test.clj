(ns aoc-2021.day-16-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-16 :refer :all]))

(deftest unit-test-day-16
  (testing "unit tests day 16"
    (testing "part 1"
      (testing "get-version-sum"
        (let [sum-fn (fn [transmission]
                       (->> transmission make-bit-holder read-packet first get-version-sum))]
          (is (= 16 (sum-fn "8A004A801A8002F478")))
          (is (= 23 (sum-fn "C0015000016115A2E0802F182340")))
          (is (= 31 (sum-fn "A0016C880162017C3686B18A3D4780")))
          ))
      )

    (testing "part 2"
      )
    ))

(deftest day-16
  (testing "day 16"
    (testing "part 1"
      (is (= 960 (day-16-1)))
      )

    (testing "part 2"
      )
    ))
