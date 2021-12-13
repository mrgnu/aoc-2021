(ns aoc-2021.day-11
  (:require [aoc-2021.utils :as utils])
  )

(def test-input-11-1
  [
   "5483143223"
   "2745854711"
   "5264556173"
   "6141336146"
   "6357385478"
   "4167524645"
   "2176841721"
   "6882881134"
   "4846848554"
   "5283751526"
   ])

(defn input-11-1 []
  (->> "resources/day_11_1.txt"
       utils/per-line-input))

(defn- char-to-int [c] (Integer. (str c)))

(defn- print-grid [grid]
  (->> grid
       (group-by (comp first first))
       (sort-by  first)
       (map second)
       (map (partial sort-by (comp second first)))
       (map (partial map second))
       (map (partial apply str))
       (map println)
       doall
       )
  nil)

(defn- read-octopus-line [row line]
  (->> line
       (map char-to-int)
       (map-indexed (fn [col power]
                      [[row col] power]))
       )
  )

(defn read-octopus-grid [lines]
  (->> lines
       (map-indexed vector)
       (reduce (fn [acc [row line]]
                 (into acc (read-octopus-line row line)))
               {})
       )
  )

(defn- adjacent-coords [coord]
  (map #(map + coord %)
       [[-1 -1] [-1  0] [-1  1]
        [ 0 -1]         [ 0  1]
        [ 1 -1] [ 1  0] [ 1  1]]))

(defn- bump-energy-levels
  ([grid] (bump-energy-levels grid (keys grid)))

  ([grid coords]
   (reduce (fn [grid coord]
             (if (contains? grid coord)
               (update grid coord inc)
               grid))
           grid
           coords))
  )

(defn- get-flash-coords [grid]
  (->> grid
       (filter (comp (partial < 9) val))
       keys
       )
  )

(defn run-iteration [grid]
  (loop [grid        (bump-energy-levels grid)
         flashed     #{} ;; coords of flashed - used to restore grid on return
         flash-count 0]
    (let [flash-coords (get-flash-coords grid)]
      (if (empty? flash-coords)
        {
         ;; no more flashes - put back flashed in grid with energy 0
         :grid        (into grid (map #(vector % 0) flashed))
         :flash-count flash-count
         }
        (let [
              ;; remove flashed from grid - this ensures they only
              ;; flash once
              grid (reduce dissoc grid flash-coords)
              ;; bump energy levels of adjacents to flashed
              ;; NOTE: this must be iterative, since adjacents are
              ;; bumped for each flash
              grid (reduce bump-energy-levels
                           grid
                           (map adjacent-coords flash-coords))
              ;; update flashed and count
              flashed     (into flashed flash-coords)
              flash-count (+ flash-count (count flash-coords))
              ]
          (recur grid flashed flash-count))))))

(defn count-flashes [n grid]
  (first
   (reduce (fn [[acc grid] _]
             (let [state (run-iteration grid)
                   acc   (+ acc (:flash-count state))]
               [acc (:grid state)]))
           [0 grid]
           (range n))))

(defn find-full-flash [grid]
  (let [octopus-count (count grid)]
    (loop [grid grid
           step 0]
      (let [step (inc step)
            {:keys [grid flash-count]} (run-iteration grid)]
        (if (= flash-count octopus-count)
          step
          (recur grid step))))))

(defn day-11-1 []
  (->> (input-11-1)
       read-octopus-grid
       (count-flashes 100)
       )
  )

(defn day-11-2 []
  (->> (input-11-1)
       read-octopus-grid
       find-full-flash
       )
  )
