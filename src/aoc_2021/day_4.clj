(ns aoc-2021.day-4
  (:require [aoc-2021.utils :as utils])
  )

(def test-input-4-1
  [
   "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
   ""
   "22 13 17 11  0"
   " 8  2 23  4 24"
   "21  9 14 16  7"
   " 6 10  3 18  5"
   " 1 12 20 15 19"
   ""
   " 3 15  0  2 22"
   " 9 18 13 17  5"
   "19  8  7 25 23"
   "20 11 10 24  4"
   "14 21 16 12  6"
   ""
   "14 21 17 24  4"
   "10 16 15  9 19"
   "18  8 23 26 20"
   "22 11 13  6  5"
   " 2  0 12  3  7"
   ])

(defn input-4-1 []
  (->> "resources/day_4_1.txt"
       utils/per-line-input))

(defn- read-draws [s]
  (as-> s x
    (clojure.string/split x #",")
    (map #(Integer. %) x)
    ))

(defn- read-bingo-row [s]
  (as-> s x
    (clojure.string/trim x)
    (clojure.string/split x #"\s+")
    (map #(Integer. %) x)
    ))

(defn read-bingo-board
  "a representation of a bingo board.
  returns a map {:nums <nums>, :rows <rows>}.
  nums is a list of all rows, as lists of ints.
  rows is a set of sets, each set representing a row or column on the
  board.
  once numbers are being drawn, any drawn number will be removed from
  any set containing it."
  [lines]
  (assert (= 5 (count lines))
          (format "expected 5 lines, got %d" (count lines)))
  (let [nums (map read-bingo-row lines)]
    (assert (every? #(= 5 (count %)) nums)
            (format "expected 5 numbers in each row"))
        (let [rows (map #(into #{} %) nums)
              cols (map (fn [r] (->> nums
                                     (map #(nth % r))
                                     (into #{})))
                        (range 5))]
          {:nums nums
           :rows (into rows cols)})))

(defn read-bingo-game [input]
  (let [draws (read-draws (first input))
        board-specs (partition 6 (rest input))
        boards (map (fn [board-spec]
                      (assert (= "" (first board-spec))
                              "expected empty line before board spec")
                      (read-bingo-board (rest board-spec)))
                    board-specs)]
    {
     :draws  draws
     :boards boards
     }))

(defn update-board [draw board]
  (let [rows
        (->> (:rows board)
             (map #(disj % draw))
             )]
    (assoc board :rows rows)))

(defn update-boards [draw boards]
  (map (partial update-board draw)
       boards))

(defn get-winners [boards]
  (filter (fn [board] (some empty? (:rows board)))
          boards))

(defn compute-score [draw board]
  (let [unmarked-sum (apply +
                            (reduce
                             (fn [unmarked row] (into unmarked row))
                             []
                             (:rows board)))
        ;;; NOTE: all unmarked numbers occur twice, since both rows and cols are kept
        unmarked-sum (/ unmarked-sum 2)]
    (* draw unmarked-sum)))

(defn play-bingo-game [{:keys [draws boards] :as game-data}]
  (loop [[draw & draws] draws
         boards boards]
    (let [boards  (update-boards draw boards)
          winners (get-winners boards)]
      (if-not (empty? winners)
        (do
          (assert (= 1 (count winners)) "expected a single winner")
          (compute-score draw (first winners)))
        (do
          (assert (not-empty draws) "no more draws to play")
          (recur draws boards))))))

(defn day-4-1 []
  (->> (input-4-1)
       read-bingo-game
       play-bingo-game
       )
  )

(defn deplete-bingo-game [{:keys [draws boards] :as game-data}]
  (loop [[draw & draws] draws
         boards boards
         winners []]
    (let [boards      (update-boards draw boards)
          new-winners (get-winners boards)
          boards      (apply disj (set boards) new-winners)
          winners     (conj winners {:draw draw :winners new-winners})]
      (if (empty? boards)
        winners
        (do
          (assert (not-empty draws) "no more draws to play")
          (recur draws boards winners))))))

(defn day-4-2 []
  (let [winners (deplete-bingo-game (read-bingo-game (input-4-1)))
        {:keys [draw winners]} (last winners)]
    (assert (= 1 (count winners)) "expected a single last winner")
    (compute-score draw (first winners))))
