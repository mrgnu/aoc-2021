(ns aoc-2021.day-12
  (:require [aoc-2021.utils :as utils]
            [clojure.set]
            )
  )

(def test-input-12-1
  [
   "start-A"
   "start-b"
   "A-c"
   "A-b"
   "b-d"
   "A-end"
   "b-end"
   ])

(def test-input-12-2
  [
   "dc-end"
   "HN-start"
   "start-kj"
   "dc-start"
   "dc-HN"
   "LN-dc"
   "HN-end"
   "kj-sa"
   "kj-HN"
   "kj-dc"
   ])

(def test-input-12-3
  [
   "fs-end"
   "he-DX"
   "fs-he"
   "start-DX"
   "pj-DX"
   "end-zg"
   "zg-sl"
   "zg-pj"
   "pj-he"
   "RW-he"
   "fs-DX"
   "pj-RW"
   "zg-RW"
   "start-pj"
   "he-WI"
   "zg-he"
   "pj-fs"
   "start-RW"
   ])

(defn input-12-1 []
  (->> "resources/day_12_1.txt"
       utils/per-line-input))

(defn is-small? [n]
  (let [s (name n)]
    (= s (.toLowerCase s))))

(defn- add-mapping [cave-map from to]
  (update cave-map from conj to))

(defn read-path-spec [line]
  (let [[s & m] (re-find #"(\w+)-(\w+)" line)]
    (assert (= 2 (count m)) "malformed path spec")
    [(keyword (first  m))
     (keyword (second m))]))

(defn read-cave-map
  "returns a map with mappings from cave to all adjacent"
  [lines]
  (->> lines
       (map read-path-spec)
       (reduce (fn [cave-map [a b]]
                 (-> cave-map
                     (add-mapping a b)
                     (add-mapping b a)))
               {})
       ))

(defn small-cave-at-most-once [visit-counts cave]
  (or (not (is-small? cave))
      (zero? (get visit-counts cave 0))))

(defn one-small-non-start-end-at-most-twice [visit-counts cave]
  (or (not (is-small? cave))
      (let [visit-count (get visit-counts cave 0)]
        (if (contains? #{:start :end} cave)
          ;; start and end only allowed once
          (zero? visit-count)
          (if (some #(> % 1)
                    (vals (filter #(is-small? (key %)) visit-counts)))
            ;; some small cave visited more than once
            (zero? visit-count)
            (< visit-count 2))))))

(defn- add-in [m k]
  (let [v (get m k 0)]
    (assoc m k (inc v))))

(defn find-paths
  "returns the set of valid paths from :start to :end"

  ;; default to small-cave-at-most-once
  ([cave-map] (find-paths small-cave-at-most-once cave-map))

  ([cave-heuristic cave-map]
   (find-paths cave-heuristic cave-map {} [] :start))

  ;; cave-huristic is a function taking visit-counts and cave,
  ;; where visit-counts is a map with mappings from cave to visit count
  ([cave-heuristic cave-map visit-counts path cave]
   (assert (cave-heuristic visit-counts cave)
           "find-paths: not allowed to visit cave")
   (let [
         ;; append currently visited cave to current path
         path (conj path cave)
         ;; update visit-counts
         visit-counts (add-in visit-counts cave)
         ]
     (if (= :end cave)
       #{ path }
       (let [to-visit (->> (cave cave-map)
                           (filter (partial cave-heuristic visit-counts))
                           )]
         (reduce (fn [paths c]
                   (let [ps (find-paths cave-heuristic
                                        cave-map
                                        visit-counts
                                        path
                                        c)]
                     (clojure.set/union paths ps)))
                 #{}
                 to-visit)))))
  )

(defn day-12-1 []
  (->> (input-12-1)
       read-cave-map
       find-paths
       count
       )
  )

(defn day-12-2 []
  (->> (input-12-1)
       read-cave-map
       (find-paths one-small-non-start-end-at-most-twice)
       count
       )
  )
