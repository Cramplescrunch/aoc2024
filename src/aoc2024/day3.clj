(ns aoc2024.day3
  (:require [clojure.string :as str]))

(def input (slurp "resources/day3.txt"))

(defn add-muls
  [input-str]
  (->> (re-seq #"mul\(([0-9]+)\,([0-9]+)\)" input-str)
       (map (fn [s] (* (Integer/parseInt (second s)) (Integer/parseInt (get s 2)))))
       (reduce +)))

(def first-answer (add-muls input))

(def second-answer
  (->> (str/split input #"don't\(\).*?do\(\)")
     (map #(add-muls %))
     (reduce +)))
