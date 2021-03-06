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
      (testing "calculate-packet"
        (let [val-fn (fn [transmission]
                       (->> transmission make-bit-holder read-packet first calculate-packet))]
          (is (=  3 (val-fn "C200B40A82")))
          (is (= 54 (val-fn "04005AC33890")))
          (is (=  7 (val-fn "880086C3E88112")))
          (is (=  9 (val-fn "CE00C43D881120")))
          (is (=  1 (val-fn "D8005AC2A8F0")))
          (is (=  0 (val-fn "F600BC2D8F")))
          (is (=  0 (val-fn "9C005AC2F8F0")))
          (is (=  1 (val-fn "9C0141080250320F1802104A08")))
          ))
      )
    ))

(deftest day-16
  (testing "day 16"
    (testing "part 1"
      (is (= 960 (day-16-1)))
      )

    (testing "part 2"
      (is (= 12301926782560 (day-16-2)))
      )
    ))
