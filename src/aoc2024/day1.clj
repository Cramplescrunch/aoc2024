(ns aoc2024.day1
  (:require [aoc2024.utils :as u]
            [clojure.string :as str]))

(def input-lists
  (let [lines (-> (slurp "resources/day1.txt")
                  (str/split-lines))]
    (->> lines
         (map (fn [line]
                (->> (str/split line #"   ")
                     (map #(Integer/parseInt %))))))))
(def first-list
  (->> input-lists
       (reduce (fn [list pair]
                 (conj list (first pair)))
               [])))

(def second-list
  (->> input-lists
       (reduce (fn [list pair]
                 (conj list (second pair)))
               [])))

(def first-answer
  (let [sorted-first-list (sort first-list)
        sorted-second-list (sort second-list)]
    (->> (map (fn [num1 num2]
                (abs (- num1 num2)))
              sorted-first-list
              sorted-second-list)
         (reduce +))))

(defn count-occurences
  [num seq]
  (->> seq
       (filter #(= num %))
       count))

;; n square time complexity, yuck...
(def second-answer
  (->> (map #(count-occurences % second-list)
            first-list)
       (map #(* %1 %2)
            first-list)
       (reduce +)))

;; O(2n) time complexity using a map to count occurences in the second list
(def freq-second-list
  (frequencies second-list))

(def xf (map #(* % (if-let [v (freq-second-list %)] v 0))))

(def optimized-second-answer
  (transduce xf + first-list))
