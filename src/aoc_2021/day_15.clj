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

(defn- bump-risk-value [risk step]
  (inc (mod (dec (+ risk step)) 9)))

(defn- create-clone [risk-levels axis step step-offset]
  (let [offset (* step-offset step)]
    (reduce (fn [acc [coord risk]]
              (assoc acc
                     (update coord axis + offset)
                     (bump-risk-value risk step)))
            {}
            risk-levels)))

(defn expand-map [risk-level-map]
  (let [old-width  (:width  risk-level-map)
        old-height (:height risk-level-map)
        new-width  (* old-width 5)
        new-height (* old-height 5)
        levels     (:risk-levels risk-level-map)
        ]
    (let [x-expanded
          (reduce (fn [acc level]
                    (let [new-tile (create-clone levels :x level old-width)]
                      (reduce conj acc new-tile)))
                  levels
                  (range 1 5))]
      (let [xy-expanded
            (reduce (fn [acc level]
                      (let [new-tile (create-clone x-expanded :y level old-height)]
                        (reduce conj acc new-tile)))
                    x-expanded
                    (range 1 5))]
        {
         :width  new-width
         :height new-height
         :risk-levels xy-expanded
         }))))

(defn- get-map-strings [risk-level-map]
  (->> risk-level-map
       :risk-levels
       ;; sort rows
       (group-by (comp :y key))
       (sort-by first)
       (map second)
       ;; sort cols
       (map (partial sort-by (comp :x key)))
       (map (partial map second))
       ;; convert to strings
       (map (partial apply str))
       ))

(defn day-15-1 []
  (->> (input-15-1)
       read-risk-level-map
       get-total-risk
       )
  )

(defn day-15-2 []
  (->> (input-15-1)
       read-risk-level-map
       expand-map
       get-total-risk
       )
  )
