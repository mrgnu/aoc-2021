(ns aoc-2021.day-8
  (:require [aoc-2021.utils :as utils])
  )

(def test-input-8-1
  [
   "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
   "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
   "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
   "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
   "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
   "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
   "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
   "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
   "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
   "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
   ])

(defn input-8-1 []
  (->> "resources/day_8_1.txt"
       utils/per-line-input))

(defn- sort-str [s]
  (apply str (sort s)))

(defn parse-segment-spec [line]
  (let [p (re-pattern (str
                       "(\\w+)\\s+"  ;;; signal pattern 0
                       "(\\w+)\\s+"  ;;; signal pattern 1
                       "(\\w+)\\s+"  ;;; signal pattern 2
                       "(\\w+)\\s+"  ;;; signal pattern 3
                       "(\\w+)\\s+"  ;;; signal pattern 4
                       "(\\w+)\\s+"  ;;; signal pattern 5
                       "(\\w+)\\s+"  ;;; signal pattern 6
                       "(\\w+)\\s+"  ;;; signal pattern 7
                       "(\\w+)\\s+"  ;;; signal pattern 8
                       "(\\w+)\\s+"  ;;; signal pattern 9
                       "\\|\\s+"     ;;; separator
                       "(\\w+)\\s+"  ;;; output value 0
                       "(\\w+)\\s+"  ;;; output value 1
                       "(\\w+)\\s+"  ;;; output value 2
                       "(\\w+)"     ;;; output value 3
                       ))
        m (re-find p line)]
    (assert (= 15 (count m)))
    (let [parts    (drop 1 m)
          sig-pats (->> parts (take 10) (map sort-str) set)
          outs     (->> parts (drop 10) (map sort-str))]
      ;;; FIXME convert to bit fields?
      {
       :signal-patterns sig-pats
       :outputs         outs
       })))

(defn parse-segment-specs [lines]
  (as-> lines x
    (map parse-segment-spec x)
    )
  )

(defn get-known-output-digit-count [segment-specs]
  (let [known-digit-lens #{
                           2 ;;; 1
                           4 ;;; 4
                           3 ;;; 7
                           7 ;;; 8
                           }]
    (->> segment-specs
         (map :outputs)
         (map (partial filter #(contains? known-digit-lens (count %))))
         (map count)
         (apply +)
         )
    ))

;; digit specs:
;;
;;  a
;; b c
;;  d
;; e f
;;  g
(def digit-specs
  {
   0 "abcefg"
   1 "cf"
   2 "acdeg"
   3 "acdfg"
   4 "bcdf"
   5 "abdfg"
   6 "abdefg"
   7 "acf"
   8 "abcdefg"
   9 "abcdfg"
   }
  )

(defn decorate-signal-pattern [signal-pattern]
  {
   :pattern signal-pattern
   }
  )

(defn decorate-signal-patterns [signal-patterns]
  (->> signal-patterns
       (map decorate-signal-pattern)
       set
       )
  )

(defn decorate-digit-spec [[digit signal-pattern]]
  (assoc (decorate-signal-pattern signal-pattern) :digit digit))

(defn decorated-digit-specs []
  (->> digit-specs
       (map decorate-digit-spec)
       set
       )
  )

(defn filter-unique
  "find unique patterns by masked pattern length - any pattern length
  that occurs exactly once can be identified"
  [decorated-signal-patterns]
  (->> decorated-signal-patterns
       (group-by (comp count :masked-pattern))
       (filter (comp (partial = 1) count second))
       (map second)
       (map first)
       ;; sorting by masked count makes sure order is same in specs
       ;; and patterns
       (sort-by (comp count :masked-pattern))
       ))

(defn mask-signal-pattern
  "masks out some digits from :pattern in a set.
  result is stored in :masked-pattern."
  [mask decorated-signal-pattern]
  (let [mask (into #{} mask)
        masked (reduce (fn [acc c]
                         (if (contains? mask c)
                           acc
                           (str acc c)))
                       ""
                       (:pattern decorated-signal-pattern)
                       )]
    (assoc decorated-signal-pattern :masked-pattern masked)))

(defn find-unique [decorated-signal-patterns
                   known-patterns]
  (some (fn [mask]
          (let [masked (map (partial mask-signal-pattern mask)
                            decorated-signal-patterns)]
            (not-empty (filter-unique masked))
            ))
        ;; this will mask with empty for first iteration
        (or (not-empty known-patterns) [""])))

(defn filter-unknown
  [decorated-signal-patterns known-patterns]
  (let [known-patterns (set known-patterns)]
    (->> decorated-signal-patterns
         (remove (comp (partial contains? known-patterns) :pattern))
         set)))

(defn collect-known
  "gets digit, stock pattern and garbled pattern from unique sets"
  [unique-specs unique-pats]
  (assert (= (count unique-specs) (count unique-pats))
          "unique: arity mismatch")
  (reduce (fn [acc [spec pat]]
            (let [digit    (:digit spec)
                  spec-pat (:pattern spec)
                  pat      (:pattern pat)]
              (assert (= (count spec-pat) (count pat))
                      "collect-known: arity mismatch")
              (conj acc {
                         :digit        digit
                         :spec-pattern spec-pat
                         :pattern      pat
                         })))
          #{}
          ;; zip
          (map vector unique-specs unique-pats)))

(defn build-digit-map
  "returns a map with mappings from garbled pattern to corresponding
  digit.
  this is done by looking for patterns of unique length in both stock
  and garbled specs. masking of known patterns is applied to unknown
  until all patterns are identified."
  [signal-patterns]
  (loop [unknown-specs (decorated-digit-specs)
         unknown-pats  (decorate-signal-patterns signal-patterns)
         known         #{}]
    (if (>= (count known) (count signal-patterns))
      (reduce (fn [acc {:keys [pattern digit]}]
                (assoc acc pattern digit))
              {}
              known)
      ;; mask out known patterns and find new unique
      (let [unique-specs (find-unique unknown-specs
                                      (map :spec-pattern known))
            unique-pats  (find-unique unknown-pats
                                      (map :pattern known))
            known (into known (collect-known unique-specs unique-pats))
            unknown-specs (filter-unknown unknown-specs
                                          (map :spec-pattern known))
            unknown-pats  (filter-unknown unknown-pats
                                          (map :pattern known))]
        (recur unknown-specs unknown-pats known)))))

(defn map-outputs [digit-spec]
  (let [digit-map (build-digit-map (:signal-patterns digit-spec))]
    (->> digit-spec
         :outputs
         (map (partial get digit-map))
         (map str)
         (apply str)
         Integer.
         )
    ))

(defn day-8-1 []
  (->> (input-8-1) parse-segment-specs get-known-output-digit-count)
  )

(defn day-8-2 []
  (->> (input-8-1) parse-segment-specs (map map-outputs) (apply +))
  )
