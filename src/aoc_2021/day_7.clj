(ns aoc-2021.day-7
  )

(def test-input-7-1
  "16,1,2,0,4,2,7,1,2,14"
  )

(defn input-7-1 []
  (->> "resources/day_7_1.txt"
       slurp
       clojure.string/trim
       )
  )

(defn read-shrimps [s]
  (as-> s x
      (clojure.string/split x #",")
      (map #(Integer. ^String %) x)
      ))

(defn- median [v]
  (let [sorted (sort v)
        n      (count sorted)]
    (let [m (int (/ n 2))]
      (if (odd? n)
        (nth sorted m)
        (int (/ (+ (nth sorted m) (nth sorted (dec m))) 2))
        )
      )))

(defn get-horizontal-position [shrimps]
  (median shrimps))

(defn get-move-cost [^Integer position shrimps]
  (reduce (fn [acc ^Integer p]
            (+ acc (Math/abs ^Integer (- position p))))
          0
          shrimps))

(defn day-7-1 []
  (let [shrimps  (->> (input-7-1) read-shrimps)
        position (get-horizontal-position shrimps)]
    (get-move-cost position shrimps)))

(defn get-acc-move-cost [^Integer from ^Integer to]
  (let [d (Math/abs ^Integer (- to from))]
    (int (/ (* d (inc d)) 2))))

(defn get-acc-fuel-cost [position shrimps]
  (reduce (fn [acc p]
            (+ acc (get-acc-move-cost p position)))
          0
          shrimps))

(defn- get-acc-horizontal-positions [shrimps]
  ;;; brute-force
  (let [sorted (sort shrimps)
        min-pos (first sorted)
        max-pos (last sorted)
        positions (range min-pos (inc max-pos))]
    (map (fn [target-pos]
           {
            :target-pos target-pos
            :fuel-cost (get-acc-fuel-cost target-pos shrimps)
            })
         positions)))

(defn get-acc-horizontal-position [shrimps]
  (let [alternatives (get-acc-horizontal-positions shrimps)]
    (->> alternatives
         (sort-by :fuel-cost)
         first
         )
    ))

(defn day-7-2 []
  (->> (input-7-1)
       read-shrimps
       get-acc-horizontal-position
       :fuel-cost
       )
  )
