(ns aoc-2021.a-star)

(defn- make-a-star-data [from to cost-fun heuristic neighbor-fun]
  {
   :from         from
   :to           to
   :open         #{from}
   :g-score      {from 0}
   :f-score      {from (heuristic from to)}
   :came-from    {}
   :heuristic    heuristic
   :cost-fun     cost-fun
   :neighbor-fun neighbor-fun
   })

(defn a-star-find-current
  ;; find next node to visit, and remove from open
  ;; returns [a-star-data current]
  [{:keys [to open f-score] :as a-star-data}]
  ;; FIXME priority queue?
  (let [[_ current]
        (reduce (fn [[min-cost min-candidate] candidate]
                  (let [cost (get f-score candidate)]
                    (if (< cost min-cost)
                      [cost candidate]
                      [min-cost min-candidate])))
                [Integer/MAX_VALUE nil]
                open)
        open (disj open current)]
    [(assoc a-star-data :open open)
     current]))

(defn a-star-update-g-score [{:keys [to g-score heuristic cost-fun] :as a-star-data}
                             c-score
                             current
                             neighbor]
  (let [cost (cost-fun current neighbor)
        g    (get g-score neighbor Integer/MAX_VALUE)
        tg   (+ c-score cost)
        ]
    (if (< tg g)
      (-> a-star-data
          (assoc-in  ,,, [:came-from neighbor] current)
          (assoc-in  ,,, [:g-score   neighbor] tg)
          (assoc-in  ,,, [:f-score   neighbor] (+ tg (heuristic neighbor to)))
          (update ,,, :open conj neighbor)
          )
      a-star-data)))

(defn- a-star-update-g-scores [{:keys [to heuristic neighbor-fun] :as a-star-data}
                               current]
  (assert (contains? (:g-score a-star-data) current))
  (let [c-score   (get (:g-score a-star-data) current)
        neighbors (neighbor-fun current)]
    (loop [a-star-data a-star-data
           neighbors   neighbors]
      (if (empty? neighbors)
        a-star-data
        (let [[neighbor & neighbors] neighbors]
          (recur (a-star-update-g-score a-star-data c-score current neighbor)
                 neighbors))))))

(defn- a-star-reconstruct-path [{:keys [from to came-from] :as a-star-data}]
  (loop [path  [to]
         coord to]
    (if (= coord from)
      (reverse path)

      (let [f (get came-from coord)]
        (recur (conj path f) f)))))

(defn a-star

  ([from to cost-fun heuristic neighbor-fun]
   (a-star (make-a-star-data from to cost-fun heuristic neighbor-fun)))

  ([{:keys [open to] :as a-star-data}]
   (when (empty? open) (throw (AssertionError. "no path found")))

   ;; find coord in open with least estimated cost and remove from open set
   (let [[a-star-data current] (a-star-find-current a-star-data)]

     ;; to reached - reconstruct path
     (if (= current to)
       (a-star-reconstruct-path a-star-data)

       ;; update g-scores
       (let [a-star-data (a-star-update-g-scores a-star-data current)]
         (recur a-star-data)))))
  )
