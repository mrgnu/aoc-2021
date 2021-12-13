(ns aoc-2021.day-10
  (:require [aoc-2021.utils :as utils])
  )

(def test-input-10-1
  [
   "[({(<(())[]>[[{[]{<()<>>"
   "[(()[<>])]({[<{<<[]>>("
   "{([(<{}[<>[]}>{[]{[(<()>"
   "(((({<>}<{<{<>}{[]{[]{}"
   "[[<[([]))<([[{}[[()]]]"
   "[{[{({}]{}}([{[{{{}}([]"
   "{<[[]]>}<{[{[{[]{()[[[]"
   "[<(<(<(<{}))><([]([]()"
   "<{([([[(<>()){}]>(<<{{"
   "<{([{{}}[<[[[<>{}]]]>[]]"
   ])

(defn input-10-1 []
  (->> "resources/day_10_1.txt"
       utils/per-line-input))

(defn parse-line [line]
  (vec line))

(defn parse-program [lines]
  (map parse-line lines))

(defn make-stack []
  (list))

(defn stack-push [stack s]
  (conj stack s))

(defn stack-pop [stack]
  [(peek stack) (pop stack)])

(defn opening-chunk? [tok]
  (contains? #{\( \[ \{ \<} tok))

(defn closing-chunk-for [tok]
  (condp = tok
    \( \)
    \[ \]
    \{ \}
    \< \>
    nil))

(defn matching-chunk? [s e]
  (let [expected (closing-chunk-for s)]
    (= e expected)))

(defn validate-program-line [prog-line]
  (loop [rem   prog-line
         stack (make-stack)]
    (if (empty? rem)
      (if (empty? stack)
        {
         :complete true
         :valid true
         }
        {
         :complete false
         :valid true
         :stack stack
         :partial prog-line
         })
      (let [[tok & rem] rem
            stack-or-err
            (if (opening-chunk? tok)
              (stack-push stack tok)
              (if (empty? stack)
                (throw (AssertionError. "spurious close not handled"))
                ;;{
                ;; :complete false
                ;; :valid false
                ;; :position (- (count prog-line) (count rem))
                ;; }
                (let [[top stack] (stack-pop stack)]
                  (if (matching-chunk? top tok)
                    stack
                    {
                     :complete false
                     :valid false
                     :position (- (count prog-line) (count rem))
                     :expected (closing-chunk-for top)
                     :found tok
                     }))))]
        (if (map? stack-or-err)
          stack-or-err
          (recur rem stack-or-err))))))

(defn get-invalid [program]
  (->> program
       (map validate-program-line)
       (filter (comp not :valid))
       )
  )

(defn illegal-token-score [tok]
  (get {
        \) 3
        \] 57
        \} 1197
        \> 25137
        }
       tok))

(defn program-error-score [program]
  (->> program
       get-invalid
       (map :found)
       (map illegal-token-score)
       (apply +)
       )
  )

(defn day-10-1 []
  (->> (input-10-1)
       parse-program
       program-error-score
       )
  )

(defn fix-program-lines [program]
  (->> program
       (map validate-program-line)
       (filter :stack)
       (map (fn [r] (concat (:partial r)
                            (map closing-chunk-for (:stack r)))))
       (map validate-program-line)
       (filter (fn [r] (or (not (:complete r))
                           (not (:valid r)))))
       )
  )

(defn completion-token-score [tok]
  (get {
        \) 1
        \] 2
        \} 3
        \> 4
        }
       tok))

(defn compute-program-line-completion-score [toks]
  (reduce (fn [acc tok]
            (+ (* 5 acc)
               (completion-token-score tok)))
          0
          toks))

(defn- middle [s]
  (assert (odd? (count s)) "count is not odd")
  (let [i (int (/ (count s) 2))]
    (nth s i)))

(defn program-completion-score [program]
  (->> program
       (map validate-program-line)
       (filter :stack)
       (map :stack)
       (map (partial map closing-chunk-for))
       (map compute-program-line-completion-score)
       sort
       middle
       )
  )

(defn day-10-2 []
  (->> (input-10-1)
       parse-program
       program-completion-score
       )
  )
