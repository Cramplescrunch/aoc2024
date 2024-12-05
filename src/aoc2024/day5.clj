(ns aoc2024.day5
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (-> (slurp "resources/day5.txt")
      (str/split #"\n\n")))

;(def input
;  (-> "47|53
;97|13
;97|61
;97|47
;75|29
;61|13
;75|53
;29|13
;97|29
;53|29
;61|53
;97|53
;61|29
;47|13
;75|47
;97|75
;47|61
;75|61
;47|29
;75|13
;53|13
;
;75,47,61,53,29
;97,61,53,29,13
;75,29,13
;75,97,47,61,53
;61,13,29
;97,13,75,29,47"
;      (str/split #"\n\n")))

(def freq-map
  (->> (first input)
       (str/split-lines)
       (map #(str/split % #"\|"))
       (map (fn [v] (map #(int (Integer/parseInt %)) v)))
       (reduce (fn [fmap pair]
                 (let [k (second pair)
                       v (first pair)]
                   (assoc fmap
                          k
                          (set/union (get fmap k) (conj #{} v)))))
               {})))

(def updates
  (->> (second input)
       (str/split-lines)
       (map #(str/split % #","))
       (map (fn [v] (map #(int (Integer/parseInt %)) v)))))

(defn get-middle
  [coll]
  (nth coll (/ (count coll) 2)))

;; To make sure every updates is uneven so that there's no surprises
;; when getting the middle value
(count (filter even? (map count updates)))

(defn is-compliant?
  [coll]
  (reduce (fn [e1 e2]
               (if (contains? (get freq-map e2) e1)
                 e2
                 (reduced false)))
             coll))

(def first-answer
  (->> updates
       (map #(if (is-compliant? %)
               (get-middle %)
               0))
       (reduce +)))

;; Part 2
(defn page-num-comparator
  [e1 e2]
  (if (contains? (get freq-map e2) e1)
    true
    false))

(def second-answer
  (->> updates
       (filter #(not (is-compliant? %)))
       (map #(sort page-num-comparator %))
       (map get-middle)
       (reduce +)))
