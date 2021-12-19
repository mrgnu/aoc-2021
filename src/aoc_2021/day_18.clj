(ns aoc-2021.day-18
  (:require [aoc-2021.utils :as utils])
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

(defn- add-left [sf-lhs n]
  (when n
    (cond
      (number? sf-lhs) (+ sf-lhs n)
      (-> sf-lhs second number?) [(first sf-lhs)
                                  (+ (second sf-lhs) n)]
      :else nil
      )))

(defn- add-right [sf-rhs n]
  (when n
    (cond
      (number? sf-rhs) (+ sf-rhs n)
      (-> sf-rhs first number?) [(+ (first sf-rhs) n)
                                 (second sf-rhs)]
      :else nil
      )))

(defn sf-explode
  ([sf-num]
   (let [[explode-state sf-num] (sf-explode sf-num 0)]
     [(some? explode-state)
      sf-num]))

  ;; NOTE: returns [exploded-pair, sf-num],
  ;; where exploded-pair is initially [lhs, rhs], but left and right
  ;; are replaced with nils when melded
  ([sf-num depth]
   (if (number? sf-num)
     ;; regular number - break recursion
     [nil sf-num]

     ;; this is a pair
     (let [[sf-lhs sf-rhs] sf-num]
       (if (< depth 4)
         ;; meld depth not reached - continue down
         (let [depth (inc depth)]
           ;; recurse left path
           (let [[left-exploded-state sf-lhs] (sf-explode sf-lhs depth)]

             (if left-exploded-state
               ;; left path exploded - no more recursion, meld as needed
               (let [exploded-right-cary (second left-exploded-state)
                     new-rhs (add-right sf-rhs exploded-right-cary)]
                 (if new-rhs
                   ;; meld to right - update right exploded state to not meld again
                   (let [left-exploded-state [(first left-exploded-state) nil]]
                     [left-exploded-state
                      [sf-lhs new-rhs]])
                   ;; no meld to right
                   [left-exploded-state [sf-lhs sf-rhs]]))

               ;; left path didn't explode - recurse right and meld as needed
               (let [[right-exploded-state sf-rhs] (sf-explode sf-rhs depth)]
                 (if-let [new-lhs (add-left sf-lhs (first right-exploded-state))]
                   ;; right path exploded
                   ;; meld to left - update left exploded state to not meld again
                   (let [right-exploded-state [nil (second right-exploded-state)]]
                     [right-exploded-state
                      [new-lhs sf-rhs]])
                   ;; no meld to left
                   [right-exploded-state [sf-lhs sf-rhs]])))))

         ;; depth == 4, melding depth
         (if (and (number? sf-lhs) (number? sf-rhs))
           ;; exploded
           [sf-num 0]
           ;; not exploded
           [nil sf-num])))))
  )

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

(defn day-18-1 []
  )

(defn day-18-2 []
  )
