(ns aoc-2021.day-1
  (:require [aoc-2021.utils :as utils])
  )

(def test-input-1-1
  [199
   200
   208
   210
   200
   207
   240
   269
   260
   263])

(defn count-depth-increases [depths]
  (->> depths
       (partition 2 1)
       (filter (fn [[a b]] (> b a)))
       count
       )
  )

(defn day-1-1 []
  (->> "resources/day_1_1.txt"
       utils/per-line-input
       (map (fn [s] (Integer. s)))
       count-depth-increases
       )
  )

(defn count-sliding-window-depth-increases [depths]
  (->> depths
       (partition 3 1)
       (map (partial apply +))
       count-depth-increases
       )
  )

(defn day-1-2 []
  (->> "resources/day_1_1.txt"
       utils/per-line-input
       (map (fn [s] (Integer. s)))
       count-sliding-window-depth-increases
       )
  )
