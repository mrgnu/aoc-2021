(ns aoc-2021.day-14-test
  (:require [clojure.test :refer :all]
            [aoc-2021.day-14 :refer :all]))

(deftest unit-test-day-14
  (testing "unit tests day 14"
    (let [polymer-spec (->> test-input-14-1 read-polymer-spec)
          {:keys [insertion-rules template]} polymer-spec]
      (testing "part 1"
        (testing "apply-polymer-spec"
          (is (= "NCNBCHB"
                 (apply-polymer-spec polymer-spec)))
          (is (= "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
                 (apply-polymer-spec 4 insertion-rules template)))
          )
        (testing "get-polymer-value"
          (is (= 1588 (get-polymer-value 10 polymer-spec)))
          )
        )

      (testing "part 2"
        (is (= 2188189693529N (get-polymer-value 40 polymer-spec)))
      )
    )))

(deftest day-14
  (testing "day 14"
    (testing "part 1"
      (is (= 5656 (day-14-1)))
      )

    (testing "part 2"
      (is (= 12271437788530N (day-14-2)))
      )
    ))
