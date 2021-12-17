(ns aoc-2021.day-17
  (:require [aoc-2021.utils :as utils])
  )

(def test-input-17-1 "target area: x=20..30, y=-10..-5")

(defn input-17-1 []
  (->> "resources/day_17_1.txt"
       slurp
       clojure.string/trim))

(defn read-target-area [input]
  (let [[s & m] (re-find #"target area: x=([+-]?\d+)\.\.([+-]?\d+), y=([+-]?\d+)\.\.([+-]?\d+)"
                         input)]
    (assert (= 4 (count m)) "malformed target area")
    (let [nums (->> m (map #(Integer. %)))
          x1 (nth nums 0)
          x2 (nth nums 1)
          y1 (nth nums 2)
          y2 (nth nums 3)
          ]
      {
       :x-min (min x1 x2)
       :x-max (max x1 x2)
       :y-min (min y1 y2)
       :y-max (max y1 y2)
       })))

(defn make-probe [xv yv]
  {
   :x  0
   :y  0
   :xv xv
   :yv yv
   })

(defn tick [{:keys [xv yv] :as probe}]
  (let [
        ;; update position
        probe (-> probe
                  (update :x + xv)
                  (update :y + yv)
                  )
        ;; compute new velocities
        xv (cond
             (= xv 0) 0
             (> xv 0) (dec xv)
             :else    (inc xv))
        yv (dec yv)
        ;; update velocities
        probe (-> probe
                  (assoc :xv xv)
                  (assoc :yv yv)
                  )
        ]
    probe))

(defn- assert-below-right [probe target-area]
  (assert (> (:y probe) (:y-max target-area))
          "code assumes target area is below probe")
  (assert (< (:x probe) (:x-min target-area))
          "code assumes target area is to right of probe")
  )

(defn simulate [target-area probe]
  (assert-below-right probe target-area)
  (loop [probe probe]
    (cond

      ;; probe below target area
      (< (:y probe) (:y-min target-area)) false

      ;; probe to right of target area
      (> (:x probe) (:x-max target-area)) false

      ;; probe is in target area
      (and
       (<= (:x-min target-area) (:x probe) (:x-max target-area))
       (<= (:y-min target-area) (:y probe) (:y-max target-area))
       )
      true

      :else (recur (tick probe)))))

(defn- sum-range [m]
  (int (/ (* m (inc m)) 2)))

(defn find-straight-down-x-velocity
  "find initial x velocity that passes through horizontal range of
  target area with x velocity 0"
  [{:keys [x-min x-max] :as target-area}]
  ;; sum(1..iv) in x-range
  ;; sum(1..iv) == (* iv (inc iv))
  (let [iv (reduce (fn [_ v]
                     (let [sx (sum-range v)]
                       (cond
                         ;; not reached
                         (< sx x-min) "not reached"
                         ;; reached
                         (<= x-min sx x-max) (reduced v)
                         ;; overshot
                         :else (reduced nil))))
                   (range))]
    (assert iv "no xv found")
    iv))

(defn find-max-height [target-area]
  (assert-below-right (make-probe 0 0) target-area)
  (let [
        ;; assume high enough trajectory that xv reaches 0
        xv (find-straight-down-x-velocity target-area)
        ;; any upwards velocity yv will return probe to y=0 with velocity (dec (- yv))
        ;; for maximum altitude, make one step from this point hit bottom of target area
        yv (dec (- (:y-min target-area)))
        ;; max height is sum(1..yv)
        max-h (sum-range yv)
        ]
    (assert (simulate target-area (make-probe xv yv))
            "didn't hit")
    max-h))

(defn find-all-trajectories [target-area]
  (let [
        min-xv 0
        max-xv (:x-max target-area)
        max-yv (dec (- (:y-min target-area)))
        min-yv (:y-min target-area)
        ]
    (let [velocities (for [x (range min-xv (inc max-xv))
                           y (range min-yv (inc max-yv))]
                       [x y])
          probes (map (partial apply make-probe) velocities)
          hits (filter (partial simulate target-area) probes)]
      (count hits))))

(defn day-17-1 []
  (->> (input-17-1)
       read-target-area
       find-max-height
       )
  )

(defn day-17-2 []
  (->> (input-17-1)
       read-target-area
       find-all-trajectories
       )
  )
