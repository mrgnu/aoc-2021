(ns aoc-2021.day-12-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-12 :refer :all]))

(deftest unit-test-day-12
  (testing "unit tests day 12"
    (testing "part 1"
      (testing "example 1"
        (let [cave-map (->> test-input-12-1 read-cave-map)
              paths    (find-paths cave-map)]
          (is (= 10 (count paths)))
          (is (= #{
                   [:start :A :b :A :c :A :end]
                   [:start :A :b :A :end]
                   [:start :A :b :end]
                   [:start :A :c :A :b :A :end]
                   [:start :A :c :A :b :end]
                   [:start :A :c :A :end]
                   [:start :A :end]
                   [:start :b :A :c :A :end]
                   [:start :b :A :end]
                   [:start :b :end]
                   }
                 paths))
          )
        )
      (testing "example 2"
        (is (= 19 (->> test-input-12-2 read-cave-map find-paths count)))
        )
      (testing "example 3"
        (is (= 226 (->> test-input-12-3 read-cave-map find-paths count)))
        )
      )

    (testing "part 2"
      )
    ))

(deftest day-12
  (testing "day 12"
    (testing "part 1"
      (is (= 4413 (day-12-1)))
      )

    (testing "part 2"
      )
    ))
