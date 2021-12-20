(ns aoc-2021.day-18
  (:require [aoc-2021.utils :as utils])
  (:require [clojure.zip :as zip])
  )

(def test-input-18-1
  [
   "[1,2]"
   "[[1,2],3]"
   "[9,[8,7]]"
   "[[1,9],[8,5]]"
   "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
   "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
   "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
   ])

(defn input-18-1 []
  (->> "resources/day_18_1.txt"
       utils/per-line-input))

(defn read-snailfish-digit [s]
  (let [[s & m] (re-find #"^(\d+)(.*)" s)]
    (assert (= 2 (count m)))
    [(second m)
     (Integer. (first m))]))

(defn read-snailfish-pair [s]
  (assert (= \[ (first s)))
  (loop [s  (apply str (rest s))
         lv nil]
    (let [[s v] (if (= \[ (first s))
                  (read-snailfish-pair s)
                  (read-snailfish-digit s))]
      (if-not lv
        (do
          (assert (= \, (first s)))
          (recur (apply str (rest s))
                 v))
        (do
          (assert (= \] (first s)))
          [(apply str (rest s))
           [lv v]])))))

(defn read-snailfish-number [line]
  (let [[rem p] (read-snailfish-pair line)]
    (assert (empty? rem))
    p))

(defn- find-num [dz step-fn]
  (loop [dz    dz
         steps 0]
    (let [dz (step-fn dz)]
      (if-not dz
        nil
        (let [steps (inc steps)
              node  (zip/node dz)]
          (if (number? node)
            [steps node]
            (recur dz steps)))))))

(defn- find-prev-num [dz]
  (when-let [p (find-num dz zip/prev)]
    (let [[steps node] p]
      [(- steps) node])))

(defn- find-next-num [dz]
  (let [next-fn (fn [dz]
                  (let [dz (zip/next dz)]
                    (when-not (zip/end? dz) dz)))]
  (find-num dz next-fn)))

(defn- update-rel [dz steps v]
  (assert steps)
  (assert (not (zero? steps)))
  (let [step-fn (if (> steps 0) zip/next zip/prev)
        back-fn (if (> steps 0) zip/prev zip/next)
        steps   (Math/abs steps)]
    ;; move to target node
    (let [dz (nth (iterate step-fn dz) steps)]
      (assert (number? (zip/node dz)))
      ;; update value
      (let [dz (zip/replace dz v)]
        ;; move back
        (nth (iterate back-fn dz) steps)))))

(defn- inc-rel [dz rel-spec v]
  (if-not rel-spec
    dz
    (let [[steps node] rel-spec
          v (+ v node)]
      (update-rel dz steps v))))

(defn- sf-explode-node [dz]
  (assert (number? (first  (zip/node dz))))
  (assert (number? (second (zip/node dz))))
  (let [node (zip/node dz)
        dz   (zip/replace dz 0)
        p    (find-prev-num dz)
        dz   (inc-rel dz p (first node))
        n    (find-next-num dz)
        dz   (inc-rel dz n (second node))]
    dz))

(defn sf-explode [sf-num]
  (if-not (vector? sf-num)
    [false sf-num]
    (let [dz
          (reduce (fn [dz _]
                    (if (zip/end? dz)
                      (reduced nil)
                      (let [depth (-> dz zip/path count)
                            node  (zip/node dz)]
                        (if (and (= depth 4)
                                 (vector? node))
                          ;; this is the one to explode
                          (reduced (sf-explode-node dz))
                          (zip/next dz)))))
                  (zip/vector-zip sf-num)
                  (range))]
      (if dz
        [true  (zip/root dz)]
        [false sf-num]))))

(defn sf-split [sf-num]
  (if (number? sf-num)
    (if (>= sf-num 10)
      (let [half (/ sf-num 2)]
        [true
         [(int (Math/floor half))
          (int (Math/ceil  half))]])
      [false sf-num])
    (let [[sf-lhs sf-rhs] sf-num
          [split? sf-lhs] (sf-split sf-lhs)]
      (if split?
        [split? [sf-lhs sf-rhs]]
        (let [[split? sf-rhs] (sf-split sf-rhs)]
          [split? [sf-lhs sf-rhs]])))))

(defn sf-reduce [sf-num]
  (loop [sf-num sf-num]
    (let [[exploded? sf-num] (sf-explode sf-num)]
      (if exploded?
        (recur sf-num)
        (let [[split? sf-num] (sf-split sf-num)]
          (if split?
            (recur sf-num)
            sf-num))))))

(defn sf-add [sf-lhs sf-rhs]
  (let [sf-num [sf-lhs sf-rhs]]
    (sf-reduce sf-num)))

(defn sf-add-all [sf-nums]
  (reduce sf-add sf-nums))

(defn sf-magnitude [sf-num]
  (if (number? sf-num)
    sf-num
    (let [[sf-lhs sf-rhs] sf-num]
      (+ (* 3 (sf-magnitude sf-lhs))
         (* 2 (sf-magnitude sf-rhs))))))

(defn day-18-1 []
  )

(defn day-18-2 []
  )
