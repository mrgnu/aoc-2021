(ns aoc-2021.day-20
  (:require [aoc-2021.utils :as utils])
  )

(defn- make-coord
  ([[x y]] (make-coord x y))
  ([x y] [x y])
  )

(defn- get-3x3 [[x y]]
  [
   [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
   [(dec x) y]       [x y]       [(inc x) y]
   [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]
   ])

(defn test-input-20-1 []
  (->> "resources/day_20_1_test.txt"
       utils/per-line-input))

(defn input-20-1 []
  (->> "resources/day_20_1.txt"
       utils/per-line-input))

(defn- read-enhancement-map [line]
  (into {} (map-indexed vector
                        line)))

(defn- read-pixel-row [y line]
  (map (fn [[x c]]
         [(make-coord x y) c])
       (map-indexed vector line)))

(defn- read-pixels [lines]
  (let [h (count lines)
        w (count (first lines))
        pixels (reduce into {}
                       (map-indexed read-pixel-row lines))]
    {
     :ox     0
     :oy     0
     :width  w
     :height h
     :pixels pixels
     }))

(defn read-image [input]
  (let [[e s & pixels] input]
    (assert (empty? s))
    (let [pixels (read-pixels pixels)
          em     (read-enhancement-map e)]
      (assoc pixels :enhancement-map em))))

(defn- get-bitmap-lines [image]
  (->> image
       :pixels
       (group-by (comp second key))
       (sort-by key)
       vals
       (map (partial group-by (comp first first)))
       (map (partial sort-by key))
       (map (partial map second))
       (map (partial map (comp second first)))
       (map (partial apply str))
       )
  )

(defn- print-bitmap [image]
  (doall (map println (get-bitmap-lines image)))
  nil)

(defn- get-coords [{:keys [ox oy width height] :as image}]
  (let [min-x ox
        min-y oy
        max-x (dec (+ min-x width))
        max-y (dec (+ min-y height))]
    (for [x (range min-x (inc max-x))
          y (range min-y (inc max-y))]
      (make-coord x y))))

(defn- enlarge-bounds [image]
  (-> image
      (update ,,, :ox dec)
      (update ,,, :oy dec)
      (update ,,, :width  + 2)
      (update ,,, :height + 2)
      ))

(defn- enhance-pixel [{:keys [enhancement-map pixels] :as image}
                      coord
                      void-state]
  (let [box (get-3x3 coord)
        o (reduce (fn [acc c]
                    (let [b (if (= \# (get pixels c void-state)) 1 0)]
                      (+ (* 2 acc) b)))
                  0
                  box)]
    (assert (<= 0 o (dec 512)))
    (get enhancement-map o)))

(defn enhance-pixels [void-state image]
  (let [pixels (reduce (fn [ps c]
                         (assoc ps c (enhance-pixel image c void-state)))
                       {}
                       (get-coords image))]
    (assoc image :pixels pixels)))

(defn enhance-image [image void-state]
  (->> image
       enlarge-bounds
       (enhance-pixels void-state)
       ))

(defn enhance-n-times [n void-states image]
  (loop [i           0
         image       image
         void-states void-states]
    (let [[void-state & void-states] void-states]
      (if (>= i n)
        image
        (recur (inc i)
               (enhance-image image void-state)
               void-states)))))

(defn day-20-1 []
  (->> (input-20-1)
       read-image
       ;; enhancement-map[0] == #, enhancement-map[511] == .
       (enhance-n-times 2 (cycle [\. \#]))
       :pixels
       vals
       (filter (partial = \#))
       count
       )
  )

(defn day-20-2 []
  )
