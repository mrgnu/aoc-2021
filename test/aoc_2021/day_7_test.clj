(ns aoc-2021.day-7-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-7 :refer :all]))

(deftest unit-test-day-7
  (testing "unit tests day 7"
    (let [shrimps (-> test-input-7-1 read-shrimps)]
      (testing "part 1"
        (testing "get-horizontal-position"
          (is (= 2 (get-horizontal-position shrimps)))
          )
        (testing "get-move-cost"
          (is (= 37 (get-move-cost 2 shrimps)))
          )
        )

      (testing "part 2"
        (testing "get-acc-move-cost"
          (is (= (get-acc-move-cost 16 5) 66))
          (is (= (get-acc-move-cost 1 5) 10))
          (is (= (get-acc-move-cost 2 5) 6))
          (is (= (get-acc-move-cost 0 5) 15))
          (is (= (get-acc-move-cost 4 5) 1))
          (is (= (get-acc-move-cost 2 5) 6))
          (is (= (get-acc-move-cost 7 5) 3))
          (is (= (get-acc-move-cost 1 5) 10))
          (is (= (get-acc-move-cost 2 5) 6))
          (is (= (get-acc-move-cost 14 5) 45))
          )
        (testing "get-acc-horizontal-position"
          (is (= {:target-pos 5 :fuel-cost 168} (get-acc-horizontal-position shrimps)))
          )
        (testing "get-acc-fuel-cost"
          (is (= 168 (get-acc-fuel-cost 5 shrimps)))
          )
        )
      )
    ))

(deftest day-7
  (testing "day 7"
    (testing "part 1"
      (is (= 349812 (day-7-1)))
      )

    (testing "part 2"
      (is (= 99763899 (day-7-2)))
      )
    ))
