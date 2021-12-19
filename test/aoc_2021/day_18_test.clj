(ns aoc-2021.day-18-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-18 :refer :all]))

(deftest unit-test-day-18
  (testing "unit tests day 18"
    (testing "part 1"
      (testing "sf-explode"
        (is (=
             [true [[[[0,9],2],3],4]]
             (sf-explode [[[[[9,8],1],2],3],4])
             ))
        (is (=
             [true [7,[6,[5,[7,0]]]]]
             (sf-explode [7,[6,[5,[4,[3,2]]]]])
             ))
        (is (=
             [true [[6,[5,[7,0]]],3]]
             (sf-explode [[6,[5,[4,[3,2]]]],1])
             ))
        (is (=
             [true [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]]
             (sf-explode [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])
             ))
        (is (=
             [true [[3,[2,[8,0]]],[9,[5,[7,0]]]]]
             (sf-explode [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]])
             ))

        (is (= [false 9]
               (sf-explode 9)))
        (is (= [false [[1 2] [3 [4 5]]]]
               (sf-explode [[1 2] [3 [4 5]]])))
        )
      )

    (testing "part 2"
      )
    ))

(deftest day-18
  (testing "day 18"
    (testing "part 1"
      )

    (testing "part 2"
      )
    ))
