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

(defn input-2-1 []
  (->> "resources/day_2_1.txt"
       utils/per-line-input))

(defn init-state []
  {
   :aim 0,
   :position {:horizontal 0, :depth 0},
   })

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

(defn move [state
            {:keys [action value] :as action}]
  (condp = action
    :forward
    (let [aim (:aim state)]
      (-> state
          (update-in ,,, [:position :horizontal] + value)
          (update-in ,,, [:position :depth] + (* aim value))))

    :up
    (update state :aim - value)

    :down
    (update state :aim + value)

    :else (assert (format "unsupported action: %s" action))
    )
  )

(defn execute-moves
  ([moves] (execute-moves moves (init-state)))

  ([moves state]
   (->> moves
        (map get-action)
        (reduce move state)
        )
   )
  )

(defn get-pos-product [{:keys [position] :as state}]
    (* (:horizontal position) (:depth position)))

(defn day-2-1 []
  (let [final-pos (->> (input-2-1)
                       execute-moves
                       )]
    (get-pos-product final-pos)))

(defn day-2-2 []
  (let [final-pos (->> (input-2-1)
                       execute-moves
                       )]
    (get-pos-product final-pos)))
