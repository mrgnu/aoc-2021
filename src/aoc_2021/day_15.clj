(ns aoc-2021.day-15
  (:require [aoc-2021.utils :as utils]
            [aoc-2021.a-star :as a-star])
  )

(defn make-coord [x y]
  {:x x :y y})

(defn get-adjacent [coord]
  #{(update coord :x inc)
    (update coord :x dec)
    (update coord :y inc)
    (update coord :y dec)})

(defn filter-adjacent [{:keys [width height]}
                       coords]
  (->> coords
       (remove (fn [{:keys [x y]}]
                 (or (neg? x) (neg? y) (>= x width) (>= y height))))
          set))

(defn- manhattan-heuristic [from to]
  (let [coords [from to]
        xs     (map :x coords)
        ys     (map :y coords)
        dx     (->> xs (apply -) Math/abs)
        dy     (->> ys (apply -) Math/abs)]
    (+ dx dy)))

(def test-input-15-1
  [
   "1163751742"
   "1381373672"
   "2136511328"
   "3694931569"
   "7463417111"
   "1319128137"
   "1359912421"
   "3125421639"
   "1293138521"
   "2311944581"
   ])

(defn input-15-1 []
  (->> "resources/day_15_1.txt"
       utils/per-line-input))

(defn read-risk-level-row [y line]
  (map-indexed (fn [x c]
                 (let [risk-level (Integer. (str c))]
                   [(make-coord x y) risk-level]))
               line))

(defn read-risk-level-map [lines]
  (let [width  (count (first lines))
        height (count lines)
        risk-levels (reduce into {} (map-indexed read-risk-level-row lines))
        ]
    {
     :width       width
     :height      height
     :risk-levels risk-levels
     }))

(defn- make-risk-level-heuristic
  "manhattan distance + risk level at from"
  [{:keys [risk-levels]}]
  (fn [from to]
    (if (= from to) 0
        (+ (dec (manhattan-heuristic from to))
           (get risk-levels from)))))

(defn- make-risk-level-cost-fun
  "risk level at to"
  [{:keys [risk-levels]}]
  (fn [from to]
    (assert (contains? (get-adjacent from) to))
    (get risk-levels to)))

(defn- make-risk-level-neighbor-fun
  "gets n,s,e,w and filters on bounds"
  [{:keys [width height] :as limits}]
  (fn [coord] (->> coord get-adjacent (filter-adjacent limits))))

(defn risk-level-a-star [risk-level-map]
  (let [from (make-coord 0 0)
        to   (make-coord (dec (:width risk-level-map)) (dec (:height risk-level-map)))]
    (a-star/a-star from
            to
            (make-risk-level-cost-fun     risk-level-map)
            (make-risk-level-heuristic    risk-level-map)
            (make-risk-level-neighbor-fun risk-level-map))))

(defn get-total-risk
  ([risk-level-map]
   (get-total-risk risk-level-map (risk-level-a-star risk-level-map)))

  ([risk-level-map path]
   (reduce (fn [acc p]
             (+ acc (get (:risk-levels risk-level-map) p)))
           0
           (rest path)))
  )

(defn day-15-1 []
  (->> (input-15-1)
       read-risk-level-map
       get-total-risk
       ))

(defn day-15-2 []
  )
