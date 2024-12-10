(ns aoc2024.day9
  (:require [clojure.string :as str]))

(def input (->> (slurp "resources/day9_test.txt")
                str/trim-newline
                (map #(Character/digit % 10))))

(defn input->point-rep
  [input]
  (->> (take (count input) (flatten (iterate (fn [v] [(inc (first v)) \.]) [0 \.])))
       (mapv #(vec [%1 %2]) input)
       (reduce (fn [acc [factor symb]]
                 (into acc (take factor (iterate identity symb))))
               [])
       vec))

(defn compact-point-rep
  [point-rep]
  (loop [i 0
         j (dec (count point-rep))
         res []]
    (cond (< j i) res
          (and (= \. (point-rep i))
               (= \. (point-rep j))) (recur i (dec j) res)
          (= \. (point-rep i)) (recur (inc i) (dec j) (into res [(point-rep j)]))
          :else (recur (inc i) j (into res [(point-rep i)])))))

;; Part 1
(time (->> (input->point-rep input)
           (compact-point-rep)
           (map-indexed (fn [idx item] (* idx item)))
           (reduce +)))

;; Part 2
;;
(def freespaces
  (->> input
       (map-indexed (fn [i v] (when (odd? i) v)))
       (filter identity)))
(def -files
  (->> input
       (map-indexed (fn [i v] (when (even? i) v)))
       (filter identity)
       (map-indexed (fn [i v]
                      (if (= 1 v)
                        (str i)
                        (reduce str (take v (iterate identity i))))))))

(defn get-reversed-files
  [files max-freespace]
  (->> files
       (filter #(<= (count %) max-freespace))
       reverse))

(get-reversed-files -files 3)

(mapv (fn [v] [v]) [1 5 6])

(defn fill-spaces
  "Returns an array of arrays of form [[x &args] [y &args]],
  x and y being the free space left and args the filled files"
  [freespaces files]
  (let [rev-files (reverse files)]
    (loop [freespaces freespaces
           rev-files (get-reversed-files files (reduce max freespaces))]
      )))
