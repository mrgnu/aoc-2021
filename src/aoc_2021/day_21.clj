(ns aoc-2021.day-21
  (:require [aoc-2021.utils :as utils]
            [clojure.math.combinatorics :as combo])
  )

(defn input-21-1 []
  (->> "resources/day_21_1.txt"
       utils/per-line-input))

(def test-input-21-1
  [
   "Player 1 starting position: 4"
   "Player 2 starting position: 8"
   ])

(defn make-player [name position]
  {
   :name     name
   :position position
   :score    0
   })

(defn read-player [line]
  (let [[s & m] (re-find #"(Player \d) starting position: (\d+)" line)]
    (assert (= 2 (count m)))
    (make-player (first m) (Integer. (second m)))))

(defn read-players [lines]
  (assert (= 2 (count lines)))
  {
   :player-1 (read-player (first lines))
   :player-2 (read-player (second lines))
   })

(defn- one-based-mod [n m]
  (inc (mod (dec n) m)))

(defn deterministic-die-seq
  ([] (deterministic-die-seq 100))
  ([n] (cycle (range 1 (inc n)))))

(defn- winner? [player winning-score]
  (>= (:score player) winning-score))

(defn update-player [player roll]
  (let [steps 10
        position (one-based-mod (+ (:position player) roll) steps)]
    (-> player
        (update ,,, :score + position)
        (assoc ,,,  :position position)
        )
    ))

(defn play-round [player die-seq]
  (let [[a b c & die-seq] die-seq
        roll (+ a b c)]
    [(update-player player roll)
     die-seq]))

(defn- other-player [player]
  (if (= :player-1 player) :player-2 :player-1))

(defn play-game
  ([die-seq players] (play-game die-seq players 1000))

  ([die-seq players winning-score]
   (loop [players players
          player  :player-1
          die-seq die-seq
          rolls   0]
     (let [[p die-seq] (play-round (get players player) die-seq)
           rolls       (+ rolls 3)
           players (assoc players player p)]
       (if (winner? p winning-score)
         (assoc players
                :winner player
                :rolls  rolls)
         (recur players
                (other-player player)
                die-seq
                rolls)))))
  )

(defn roll-permutations
  "returns a map with keys as possible roll sums and vals as number of
  combinations for that roll sum"
  [roll-vals roll-count]
  (as-> roll-vals x
    (combo/selections x roll-count)
    (group-by (partial apply +) x)
    (reduce (fn [acc [v vs]]
              (assoc acc v (count vs)))
            {}
            x)
    )
  )

(defn play-games
  ([players] (play-games (roll-permutations [1 2 3] 3)
                         players
                         :player-1
                         21
                         1
                         {:player-1 0N :player-2 0N}
                         ))

  ([roll-values players player winning-score perm-count wins]
   (let [p (get players player)]
     (reduce (fn [wins [rs rc]]
               (let [rp (update-player p rs)
                     players (assoc players player rp)
                     pc (* perm-count rc)]
                 (if (winner? rp winning-score)
                   (update wins player + pc)
                   (play-games roll-values
                               players
                               (other-player player)
                               winning-score
                               pc
                               wins))))
             wins
             roll-values)))
  )

(defn compute-game-score [players]
  (let [loser (get players (other-player (:winner players)))]
    (* (:rolls players)
       (get loser :score))))

(defn day-21-1 []
  (->> (input-21-1)
       read-players
       (play-game (deterministic-die-seq))
       compute-game-score
       )
  )

(defn day-21-2 []
  (->> (input-21-1)
       read-players
       play-games
       vals
       (apply max)
       )
  )
