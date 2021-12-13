(ns aoc-2021.day-9
  (:require [aoc-2021.utils :as utils])
  )

(def test-input-9-1
  [
   "2199943210"
   "3987894921"
   "9856789892"
   "8767896789"
   "9899965678"
   ])

(defn input-9-1 []
  (->> "resources/day_9_1.txt"
       utils/per-line-input))

(defn read-height-line [line]
  (->> line
       (map str)
       (map #(Integer. ^String %))
       int-array
       )
  )

(defn read-height-map [lines]
  (let [h (count lines)
        w (count (first lines))]
    {
     :width  w
     :height h
     :data   (into-array (map read-height-line lines))
     }))

(defn- adjacent-coords [row col]
  (map (fn [[dr dc]] [(+ row dr) (+ col dc)])
       [[-1 0] [1 0] [0 -1] [0 1]]))

(defn- get-coords [{:keys [width height] :as height-map}]
  (for [row (range height)
        col (range width)]
    [row col]))

(defn adjacent [[row col] {:keys [width height data] :as height-map}]
  (let [adj (adjacent-coords row col)
        adj (filter (fn [[row col]]
                      (and (>= row 0) (< row height)
                           (>= col 0) (< col width)))
                    adj)]
    (map (partial apply aget data) adj)))

(defn find-low-points [{:keys [data] :as height-map}]
  (let [coords (get-coords height-map)]
    (reduce (fn [acc coord]
              (let [h (apply aget data coord)
                    a (adjacent coord height-map)]
                (if (every? #(< h %) a)
                  (conj acc h)
                  acc)))
            []
            coords)))

(defn risk-level [h]
  (inc h))

(defn day-9-1 []
  (->> (input-9-1)
       read-height-map
       find-low-points
       (map risk-level)
       (apply +)
       )
  )

(defn day-9-2 []
  )
