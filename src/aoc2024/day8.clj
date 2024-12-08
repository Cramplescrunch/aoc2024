(ns aoc2024.day8
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(def input
  (->> (slurp "resources/day8.txt")
       (str/split-lines)
       (mapv (comp vec seq))))

(def width (count (first input)))
(def height (count input))
(def locations (set (for [y (range height) x (range width)] [y x])))
(def antennas (reduce (fn [acc loc]
                        (let [c (get-in input loc)]
                          (cond-> acc
                            (not= \. c)
                            (assoc c (set/union (get acc c) #{loc})))))
                      {}
                      locations))

(defn get-distance-vector
  [[y1 x1 :as p1]
   [y2 x2 :as p2]]
  [(- y2 y1) (- x2 x1)])

(defn invert
  [[y x :as point]]
  [(- y) (- x)])

(defn get-end-point
  [[yd xd :as distance]
   [ys xs :as start]]
  [(+ ys yd) (+ xs xd)])

(defn get-antinodes
  [[ya1 xa1 :as a1]
   [ya2 xa2 :as a2]]
  (let [distance (get-distance-vector a1 a2)]
    [(get-end-point (invert distance) a1)
     (get-end-point distance a2)]))

(defn is-antinode-valid?
  [antinode]
  (locations antinode))

(defn find-antenna-antinodes
  [locations get-antinodes-fn]
  (let [combinations (combo/combinations locations 2)]
    (->> combinations
         (map (fn [[a1 a2]] (get-antinodes-fn a1 a2)))
         (apply concat) ; used for one level flattening
         (filter is-antinode-valid?))))

(find-antenna-antinodes (get antennas \A))

;; Part 1
(->> (keys antennas)
     (map #(find-antenna-antinodes (get antennas %) get-antinodes))
     (apply concat)
     distinct
     count)

;; Part 2
(defn get-resonant-antinodes
  [[ya1 xa1 :as a1]
   [ya2 xa2 :as a2]]
  (let [distance (get-distance-vector a1 a2)]
    (concat (take-while is-antinode-valid? (iterate (partial get-end-point (invert distance)) a1))
            (take-while is-antinode-valid? (iterate (partial get-end-point distance) a2)))))

(->> (keys antennas)
     (map #(find-antenna-antinodes (get antennas %) get-resonant-antinodes))
     (apply concat)
     distinct
     count)
