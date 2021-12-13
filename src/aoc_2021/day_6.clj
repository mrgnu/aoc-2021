(ns aoc-2021.day-6
  (:require [aoc-2021.utils :as utils])
  )

(def test-input-6-1
  "3,4,3,1,2"
  )

(defn input-6-1 []
  (->> "resources/day_6_1.txt"
       slurp
       clojure.string/trim))

(defn read-fish [s]
  (as-> s x
    (clojure.string/split x #",")
      (map #(Integer. %) x)
      )
  )

(defn count-fish-lifeline [^Integer days-to-count ^Integer days-until-spawn]
  (if (<= days-to-count days-until-spawn)
    1N
    (let [days-left (- days-to-count days-until-spawn)]
      (+ ^BigInteger (count-fish-lifeline days-left 9)
         ^BigInteger (count-fish-lifeline days-left 7)))))

(defn count-fish-lifelines-r [^Integer days-to-count ^ints fish]
  (reduce (fn [acc days-until-spawn]
            (+ acc (count-fish-lifeline days-to-count days-until-spawn)))
          0
          fish))

(defn inc-in-map
  ([m k] (inc-in-map m k 1))
  ([m k i]
   (let [v (get m k 0N)]
     (assoc m k (+ v i)))))

(defn- count-fish-lifelines-i [^Integer days-to-count ^ints fish]
  (let [timer-map (reduce inc-in-map {} fish)]
    (loop [timer-map timer-map
           day       0]
      (if (>= day days-to-count)
        (apply + (vals timer-map))
        (let [timer-map
              (reduce
               (fn [tm [k v]]
                 (let [k (dec k)]
                   (if (< k 0)
                     (-> tm
                         (inc-in-map ,,, 6 v)
                         (inc-in-map ,,, 8 v))
                     (inc-in-map tm k v))))
               {}
               timer-map)]
          (recur timer-map (inc day)))))))

(defn count-fish-lifelines [^Integer days-to-count ^ints fish]
  (count-fish-lifelines-i days-to-count fish))

(defn day-6-1 []
  (->> (input-6-1)
       read-fish
       (count-fish-lifelines 80)
   )
  )

(defn day-6-2 []
  (->> (input-6-1)
       read-fish
       (count-fish-lifelines 256)
   )
  )
