(ns aoc-2021.day-9-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-9 :refer :all]))

(deftest unit-test-day-9
  (testing "unit tests day 9"
    (testing "part 1"
      (is (= 15 (->> test-input-9-1 read-height-map find-low-points (map risk-level) (apply +))))
      )

    (testing "part 2"
      (is (= 1134 (->> test-input-9-1
                       read-height-map
                       find-basins
                       (sort-by count)
                       reverse
                       (take 3)
                       (map count)
                       (apply *)
                       )
             ))
      )
    ))

(deftest day-9
  (testing "day 9"
    (testing "part 1"
      (is (= 564 (day-9-1)))
      )

    (testing "part 2"
      (is (= 1038240 (day-9-2)))
      )
    ))
