(ns aoc-2021.day-2-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-2 :refer :all]))

(deftest unit-test-day-2
  (testing "unit tests day 2"
    (comment ;;; NOTE: part 2 changes interpretation
      (testing "part 1"
        (let [final-pos (execute-moves test-input-2-1)]
          (testing "execute-moves"
            (is (= {:horizontal 15, :depth 10} final-pos)))
          (testing "get-pos-product"
            (is (= 150 (get-pos-product final-pos))))
          ))
      )

      (testing "part 2"
        (let [final-pos (execute-moves test-input-2-1)]
          (testing "execute-moves"
            (is (= {:aim 10, :position {:horizontal 15, :depth 60}} final-pos)))
          (testing "get-pos-product"
            (is (= 900 (get-pos-product final-pos))))
          ))
    ))

(deftest day-1
  (testing "day 1"
    (comment ;;; NOTE: part 2 changes interpretation
      (testing "part 1"
        (let [final-pos (execute-moves (input-2-1))]
          (testing "execute-moves"
            (is (= {:horizontal 2003, :depth 872} final-pos)))
          (testing "get-pos-product"
            (is (= 1746616 (get-pos-product final-pos))))
          ))
      )

    (testing "part 2"
      (let [final-pos (execute-moves (input-2-1))]
        (testing "execute-moves"
          (is (= {:aim 872, :position {:horizontal 2003, :depth 869681}} final-pos)))
        (testing "get-pos-product"
          (is (= 1741971043 (get-pos-product final-pos))))
        ))
    ))
