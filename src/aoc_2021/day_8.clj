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
          sig-pats (->> parts (take 10) (map sort-str) into #{})
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

(defn day-8-1 []
  (->> (input-8-1) parse-segment-specs get-known-output-digit-count)
  )

(defn day-8-2 []
  )
