(ns aoc-2021.day-2
  (:require [aoc-2021.utils :as utils])
  )

(def test-input-2-1
  ["forward 5"
   "down 5"
   "forward 8"
   "up 3"
   "down 8"
   "forward 2"])

(def input-2-1
  (->> "resources/day_2_1.txt"
       utils/per-line-input))

(defn get-action [s]
  (let [[action value] (clojure.string/split s #"\s+")
        value (Integer. value)]
    (condp = action
      "forward"
      {:action :forward,
       :value  value}

      "down"
      {:action :down,
       :value  value}
      "up"
      {:action :up,
       :value  value}
      :else (assert (format "unsupported action: '%s'" s))
      )
    )
  )

(defn move [position
            {:keys [action value] :as action}]
  (condp = action
    :forward
    (update position :horizontal + value)

    :up
    (update position :depth - value)

    :down
    (update position :depth + value)

    :else (assert (format "unsupported action: %s" action))
    )
  )

(defn execute-moves
  ([moves] (execute-moves moves {:horizontal 0 :depth 0}))

  ([moves position]
   (->> moves
        (map get-action)
        (reduce move position)
        )
   )
  )

(defn get-pos-product [{:keys [horizontal depth] :as position}]
    (* horizontal depth))

(defn day-2-1 []
  (let [final-pos (->> input-2-1
                       execute-moves
                       )]
    (get-pos-product final-pos)))
