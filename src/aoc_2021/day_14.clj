(ns aoc-2021.day-14
  (:require [aoc-2021.utils :as utils])
  )

(def test-input-14-1
  [
   "NNCB"
   ""
   "CH -> B"
   "HH -> N"
   "CB -> H"
   "NH -> C"
   "HB -> C"
   "HC -> B"
   "HN -> C"
   "NN -> C"
   "BH -> H"
   "NC -> B"
   "NB -> B"
   "BN -> B"
   "BB -> N"
   "BC -> B"
   "CC -> N"
   "CN -> C"
   ])

(defn input-14-1 []
  (->> "resources/day_14_1.txt"
       utils/per-line-input))

(defn- read-insertion-rule [line]
  (let [[s & m] (re-find #"(\w{2}) -> (\w)" line)]
    (assert (= 2 (count m)) "malformed intertion rule")
    {
     :pair   (first m)
     :result (second m)
     }))

(defn read-polymer-spec [lines]
  (let [[template blank & insertion-rules] lines]
    (assert (empty? blank) "malformed polymer spec")
    {
     :insertion-rules (->> insertion-rules
                           (map read-insertion-rule)
                           ;; to pair -> result map
                           (map vals)
                           (reduce (partial apply assoc) {})
                           )
     :template template
     }))

(defn apply-polymer-spec
  ([{:keys [insertion-rules template] :as polymer-spec}]
   (apply-polymer-spec 1 insertion-rules template))

  ([n insertion-rules template]
   (let [ending (last template)]
     (loop [i       0
            polymer template]
       (if (>= i n)
         polymer
         (let [pairs (->> polymer (partition 2 1) (map (partial apply str)))
               polymer
               (str (reduce (fn [acc p]
                              (let [c (get insertion-rules p)]
                                (str acc (first p) c)))
                            ""
                            pairs)
                    ending)]
           (recur (inc i) polymer))))))
  )

(defn- get-char-counts [s]
  (let [occurrences (group-by identity s)]
        (reduce (fn [m k]
                  (update m k (comp bigint count)))
                occurrences
                (keys occurrences))))

(defn get-polymer-value

  ([n polymer-spec]
   (let [polymer (apply-polymer-spec n (:insertion-rules polymer-spec) (:template polymer-spec))]
     (get-polymer-value polymer)))

  ([polymer]
   (let [char-counts (sort-by second (get-char-counts polymer))
         min-pair (first char-counts)
         max-pair (last char-counts)
         ]
     (- (second max-pair) (second min-pair))))
  )

(defn day-14-1 []
  (->> (input-14-1)
       read-polymer-spec
       (get-polymer-value 10)
       )
  )

(defn day-14-2 []
  )
