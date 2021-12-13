(ns aoc-2021.day-3
  (:require [aoc-2021.utils :as utils])
  )

(def test-input-3-1
  [
   "00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"
   ])

(defn- to-bits
  "converts a string to 'bits' (aka ints)"
  [s]
  (reduce (fn [vs c]
            (conj vs (Integer. (str c))))
          []
          s))

(defn- from-bits
  "converts a string of 'bits' (aka ints) to an int"
  [bits]
  (->> bits
       (reduce (fn [acc b] (+ (* 2 acc) b)) 0)
       )
  )

(defn- get-bit-row [report n]
  (map #(nth % n) report))

(defn- get-bit-counts
  "returns a sequence of [<bit> <count>] pairs"
  [bit-row]
  (->> bit-row
       (group-by identity)
       (map (fn [[k bits]] [k (count bits)]))))

(defn- most-prominent-bit
  "determines and returns the bit in a bit-row that has the most occurences.
  if 0s and 1s are equally represented, `tie-selector-fn' is called with bit-counts."
  ([bit-row]
   (most-prominent-bit bit-row (fn [_] (throw (AssertionError. "equal amount of 1s and 0s"))))
   )

  ([bit-row tie-selector-fn]
  (let [bit-counts (get-bit-counts bit-row)]
    (assert (= 2 (count bit-counts)) "bits are not bits")
    (if (= (second (first bit-counts)) (second (second bit-counts)))
      (tie-selector-fn bit-row)
      (->> bit-counts
           (sort-by second)
           reverse
           first
           first
           )
      ))
   )
  )

(defn make-report
  "converts strings in input to vecs of ints"
  [input]
  (let [report (map to-bits input)
        [len & lens] (map count report)]
    (assert (every? #(= % len) lens) "not a valid report")
    report))

(defn get-gamma-rate [report bit-count]
  (assert (validate-diagnostic-report report) "not a valid report")
  (let [bit-rows (map (partial get-bit-row report) (range bit-count))]
    (->> bit-rows
         (map most-prominient-bit)
         from-bits
         )
    ))

(defn get-epsilon-rate [gamma-rate bit-count]
  (- (dec (int (Math/pow 2 bit-count))) gamma-rate))

(defn get-power-consumption [report]
  (let [bit-count ((comp count first) report)
        gamma     (get-gamma-rate report bit-count)
        epsilon   (get-epsilon-rate gamma bit-count)]
    (* gamma epsilon)))

(defn input-3-1 []
  (->> "resources/day_3_1.txt"
       utils/per-line-input
       )
  )

(defn day-3-1 []
  (->> (input-3-1)
       make-report
       get-power-consumption
       )
  )

(defn day-3-2 []
  )
