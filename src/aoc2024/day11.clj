(ns aoc2024.day11
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def test-input "125 17")

(def input
  (-> (slurp "resources/day11.txt")
      str/trim-newline
      (str/split #" ")))

(defn split-stone
  [stone]
  (let [middle (/ (count stone) 2)
        first-half (subs stone 0 middle)
        actual-first-half (str/replace first-half #"^0+(?=\d)" "")
        second-half (subs stone middle)
        actual-second-half (str/replace second-half #"^0+(?=\d)" "")]
    [actual-first-half actual-second-half]))

(str/replace "000" #"^0+(?=\d)" "")

(defn multiply-by-2024
  [stone]
  (str (* 2024 (parse-long stone))))

(defn update-stone
  [stone]
  (cond
    (= "0"  stone)
    "1"

    (even? (count stone))
    (split-stone stone)

    :else
    (multiply-by-2024 stone)))

(defn blink
  [stones]
  (->> (map update-stone stones)
       flatten))

;; Part 1
(time
 (count (nth (iterate blink input) 25)))

;; Part 1 is cute but REALLY naive and inefficient
;; TODO For part 2, try dealing with numbers instead of strings
;; and use memoization to avoid computing the same results
;; multiple times (try to reimplement bhauman's solution again)
(def input2
  (edn/read-string (str "(" (slurp "resources/day11.txt") ")")))

(defn blink-stone
  [stone]
  (if (zero? stone)
    1
    (let [stone-str (str stone)
          stone-str-size (count stone-str)
          middle-str (/ stone-str-size 2)]
      (if (even? stone-str-size) [(parse-long (subs stone-str 0 middle-str))
                                  (parse-long (subs stone-str middle-str))]
          (* 2024 stone)))))

(def n-times-blinker
  (memoize
   (fn [n stone]
     (if (zero? n)
       1
       (let [blinked-stone (blink-stone stone)]
         (if (number? blinked-stone)
           (n-times-blinker (dec n) blinked-stone)
           (+ (n-times-blinker (dec n) (first blinked-stone))
              (n-times-blinker (dec n) (second blinked-stone)))))))))

;; part 1 revamped
(time
 (->> (map (partial n-times-blinker 25) input2)
      (reduce +)))

;; Part 2
(time
 (->> (map (partial n-times-blinker 75) input2)
      (reduce +)))

;; TODO try also with using frequency map
