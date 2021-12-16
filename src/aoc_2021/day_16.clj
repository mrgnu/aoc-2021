(ns aoc-2021.day-16
  (:require [aoc-2021.utils :as utils])
  )

;; number: [v6 t4], 2021
(def test-input-16-1 "D2FE28")

;; operator: [v1 t6], len=27 bits, [lv 10, lv 20]
(def test-input-16-2 "38006F45291200")

;; operator: [v7 t3], n=3 packages, [lv 1, lv 2, lv 3]
(def test-input-16-3 "EE00D40C823060")

(defn input-16-1 []
  (->> "resources/day_16_1.txt"
       slurp
       clojure.string/trim
       ))

(defn- to-int [bits]
  (reduce (fn [acc b]
            (+ (* 2 acc) b))
          0
          bits))

(defn- get-bits [^Integer i]
  (reduce (fn [bits e]
            (conj bits
                  (if (zero? (bit-and (int (Math/pow 2 e)) i)) 0 1)))
          []
          (reverse (range 8))))

(defn make-bit-holder [hex-str]
  (assert (zero? (mod (count hex-str) 2)))
  (let [bits
        (reduce (fn [acc hex-pair]
                  (let [bs (apply str "0x" hex-pair)
                        i  (Integer/decode bs)]
                    (into acc (get-bits i))))
                []
                (partition 2 hex-str))]
    {
     :bits   bits
     :offset 0
     :stream bits
     }
    ))

(defn advance [n bit-holder]
  (-> bit-holder
      (update ,,, :stream (partial drop n))
      (update ,,, :offset + n)
      ))

(defn eat [n {:keys [stream] :as bit-holder}]
  (let [bits (take n stream)]
    [bits
     (advance n bit-holder)]))

(defn read-value [n {:keys [stream] :as bit-holder}]
  (let [[bits bit-holder] (eat n bit-holder)]
    [(to-int bits)
     bit-holder]))

(defn read-header [bit-holder]
  (let [[ver  bit-holder] (read-value 3 bit-holder)
        [type bit-holder] (read-value 3 bit-holder)]
    [{
      :version ver
      :type    type
      }
     bit-holder]))

(defn read-literal-value [bit-holder]
  (loop [bit-holder bit-holder
         value-bits []]
    (let [[prefix bit-holder] (read-value 1 bit-holder)
          [chunk  bit-holder] (eat        4 bit-holder)]
      (let [value-bits (into value-bits chunk)]
        (if (zero? prefix)
          [(to-int value-bits) bit-holder]
          (recur bit-holder value-bits))))))

(declare read-packet)

(defn read-to-length [len bit-holder]
  (let [start-offset (:offset bit-holder)
        end-offset   (+ start-offset len)]
    (loop [bit-holder bit-holder
           packets    []]
      (assert (<= (:offset bit-holder) end-offset) "read-to-length: too much read")
      (if (= (:offset bit-holder) end-offset)
        [packets bit-holder]
        (let [[packet bit-holder] (read-packet bit-holder)
              packets (conj packets packet)]
          (recur bit-holder packets))))))

(defn read-n [n bit-holder]
  (reduce (fn [[packets bit-holder] _]
            (let [[packet bit-holder] (read-packet bit-holder)
                  packets (conj packets packet)]
              [packets bit-holder]))
          [[] bit-holder]
          (range n)))

(defn read-operator [bit-holder]
  (let [[length-type-id bit-holder] (read-value 1 bit-holder)]
    (if (zero? length-type-id)
      ;; 15 bits - total length in bits
      (let [[total-length bit-holder] (read-value 15 bit-holder)]
        (read-to-length total-length bit-holder))
      ;; 11 bits - number of sub-packets
      (let [[sub-packet-count bit-holder] (read-value 11 bit-holder)]
        (read-n sub-packet-count bit-holder)))))

(defn read-packet
  ([bit-holder] (apply read-packet (read-header bit-holder)))

  ([header bit-holder]
   (let [[packet bit-holder]
         (condp = (:type header)
           ;; 4: literal value
           4 (read-literal-value bit-holder)
           ;; else: operator
           (read-operator bit-holder)
           )]
     [{
       :header header
       :packet packet
       }
      bit-holder]))
  )

(defn get-version-sum [packet]
  (let [type    (get-in packet [:header :type])
        version (get-in packet [:header :version])]
    (condp = type
      ;; 4: literal value
      4 version
      ;; else: operator
      (let [sub-packages (:packet packet)]
        (+ version
           (apply + (map get-version-sum sub-packages))))
      )))

(defn calculate-packet [packet]
  (let [type  (get-in packet [:header :type])
        value (get packet :packet)]
    (condp = type
      ;; 0: sum
      0 (apply + (map calculate-packet value))
      ;; 1: product
      1 (apply * (map calculate-packet value))
      ;; 2: min
      2 (apply min (map calculate-packet value))
      ;; 3: max
      3 (apply max (map calculate-packet value))
      ;; 4: literal value
      4 value
      ;; 5: greater than
      5 (if (apply > (map calculate-packet value)) 1 0)
      ;; 6: less than
      6 (if (apply < (map calculate-packet value)) 1 0)
      ;; 7: equal
      7 (if (apply = (map calculate-packet value)) 1 0)
      )))

(defn day-16-1 []
  (->> (input-16-1)
       make-bit-holder
       read-packet
       first
       get-version-sum
       )
  )

(defn day-16-2 []
  (->> (input-16-1)
       make-bit-holder
       read-packet
       first
       calculate-packet
       )
  )
