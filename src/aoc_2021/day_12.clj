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

(defn find-paths
  "returns the set of valid paths from :start to :end"

  ([cave-map] (find-paths cave-map #{} [] :start))

  ([cave-map visited-small path cave]
   (assert (not (contains? visited-small cave)))
   (let [
         ;; append currently visited cave to current path
         path (conj path cave)
         ;; update visited-small
         visited-small (if (is-small? cave) (conj visited-small cave) visited-small)
         ]
     (if (= :end cave)
       #{ path }
       (let [to-visit (->> (cave cave-map)
                           (remove
                            (fn [c] (and (is-small? c)
                                         (contains? visited-small c)))))]
         (reduce (fn [paths c]
                   (let [ps (find-paths cave-map visited-small path c)]
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
  )
