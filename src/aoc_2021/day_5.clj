(ns aoc-2021.day-5
  (:require [aoc-2021.utils :as utils])
  )

(def test-input-5-1
  [
   "0,9 -> 5,9"
   "8,0 -> 0,8"
   "9,4 -> 3,4"
   "2,2 -> 2,1"
   "7,0 -> 7,4"
   "6,4 -> 2,0"
   "0,9 -> 2,9"
   "3,4 -> 1,4"
   "0,0 -> 8,8"
   "5,5 -> 8,2"
   ])

(defn input-5-1 []
  (->> "resources/day_5_1.txt"
       utils/per-line-input))

(defn make-coord
  "a two-dimensional coord"
  [x y]
  {:x x, :y y})

(defn make-vent-map
  "a map of vents - key is coord, val is vent count"
  []
  {})

(defn read-line-spec [s]
  (let [m (re-find #"(\d+),(\d+)\s+->\s+(\d+),(\d+)" s)]
    (assert (= 5 (count m)) "invalid line spec")
    {
     :fx (Integer. (nth m 1))
     :fy (Integer. (nth m 2))
     :tx (Integer. (nth m 3))
     :ty (Integer. (nth m 4))
     }))

(defn make-vent-line
  ([{:keys [fx fy tx ty] :as line-spec}]
   (make-vent-line [fx fy] [tx ty])
   )

  ([[fx fy] [tx ty]]
   (let [[fx tx] (sort [fx tx])
         [fy ty] (sort [fy ty])]
     (cond

       ;;; horizontal
       (= fx tx)
       (map #(make-coord fx %) (range fy (inc ty)))

       ;;; vertical
       (= fy ty)
       (map #(make-coord % fy) (range fx (inc tx)))

       ;;; diagonal
       :else nil
       ))
   )
  )

(defn read-lines [input]
  (->> input
       (map read-line-spec)
       (map make-vent-line)
       (filter identity)
       ))

(defn update-vent-map [vent-map vent-line]
  (reduce (fn [vent-map coord]
            (let [vent-count (get vent-map coord 0)]
              (assoc vent-map coord (inc vent-count))))
          vent-map
          vent-line))

(defn build-vent-map [vent-lines]
  (reduce update-vent-map
          (make-vent-map)
          vent-lines))

(defn count-vents [min-vent-count vent-map]
  (->> vent-map
       (filter (fn [[k v]] (>= v min-vent-count)))
       count
       ))

(defn day-5-1 []
  (->> (input-5-1)
       read-lines
       build-vent-map
       (count-vents 2)
       ))

(defn day-5-2 []
  )
