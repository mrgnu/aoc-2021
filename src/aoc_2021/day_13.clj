(ns aoc-2021.day-13
  (:require [aoc-2021.utils :as utils])
  )

(def test-input-13-1
  [
   "6,10"
   "0,14"
   "9,10"
   "0,3"
   "10,4"
   "4,11"
   "6,0"
   "6,12"
   "4,1"
   "0,13"
   "10,12"
   "3,4"
   "3,0"
   "8,4"
   "1,10"
   "2,14"
   "8,10"
   "9,0"
   ""
   "fold along y=7"
   "fold along x=5"
   ])

(defn input-13-1 []
  (->> "resources/day_13_1.txt"
       utils/per-line-input))

(defn- make-dot [[x y]]
  {:x x :y y})

(defn- read-dot [line]
  (let [[s & m] (re-find #"(\d+),(\d+)" line)]
    (assert (= 2 (count m)) "invalid dot line")
    (->> m (map #(Integer. %)) make-dot)))

(defn- read-fold [line]
  (let [[s & m] (re-find #"fold along (\w)=(\d+)" line)]
    (assert (= 2 (count m)) "invalid fold line")
    {
     :axis (keyword (first m))
     :pos  (Integer. (second m))
     }))

(defn read-manual [lines]
  (let [dots  (take-while (comp not empty?) lines)
        folds (drop (inc (count dots)) lines)]
    {
     :dots  (->> dots (map read-dot) set)
     :folds (map read-fold folds)
     }))

(defn- dot-row-str [width dot-row]
  (let [dot-set (set dot-row)
        s       (reduce (fn [acc x]
                          (str acc (if (contains? dot-set x) "#" ".")))
                        ""
                        (range width))]
    s))

(defn manual-strings [dots]
  (let [width (inc (->> dots (map :x) (apply max)))]
    (->> dots
         (group-by :y)
         (sort-by key)
         vals
         (map (partial map :x))
         (map sort)
         (map (partial dot-row-str width))
         )
    ))

(defn- to-printable [s]
  (-> s
      (clojure.string/replace #"#" "\u2588")
      (clojure.string/replace #"\." " ")
      ))

(defn- print-manual [manual-strings]
  (->> manual-strings
       (map to-printable)
       (map println)
       doall
       )
  nil)

(defn fold-at [fold-val axis dots]
  (let [grouped (group-by #(<= (axis %) fold-val) dots)
        folded  (get grouped false)
        ;; remove folded from dot set
        dots (reduce disj dots folded)
        ;; mirror folded dots
        mirrored (map (fn [dot]
                        (let [c (axis dot)
                              c (- (* 2 fold-val) c)]
                          (assoc dot axis c)))
                      folded)
        ;; add mirrored to dot set
        dots (reduce conj dots mirrored)
        ]
    dots
    ))

(defn perform-fold [manual]
  (let [dots               (:dots  manual)
        [fold & folds]     (:folds manual)
        {:keys [axis pos]} fold
        dots (fold-at pos axis dots)]
    {
     :dots  dots
     :folds folds
     }))

(defn fold-all [manual]
  (loop [manual manual]
    (if (empty? (:folds manual))
      manual
      (recur (perform-fold manual)))))

(defn day-13-1 []
  (->> (input-13-1)
       read-manual
       perform-fold
       :dots
       count
       )
  )

(defn day-13-2 []
  (->> (input-13-1)
       read-manual
       fold-all
       :dots
       manual-strings
       )
  )
