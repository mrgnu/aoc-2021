(ns aoc-2021.day-2-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-2 :refer :all]))

(deftest unit-test-day-2
  (testing "unit tests day 2"
    (testing "part 1"
      (let [final-pos (execute-moves test-input-2-1)]
        (testing "execute-moves"
          (is (= {:horizontal 15, :depth 10} final-pos)))
        (testing "get-pos-product"
          (is (= 150 (get-pos-product final-pos))))
        ))
    ))

(deftest day-1
  (testing "day 1"
    (testing "part 1"
      (let [final-pos (execute-moves input-2-1)]
        (testing "execute-moves"
          (is (= {:horizontal 2003, :depth 872} final-pos)))
        (testing "get-pos-product"
          (is (= 1746616 (get-pos-product final-pos))))
        ))
    ))
