(ns aoc2024.day6v2
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; trying to implement bhauman solution by memory for educational purposes

(def input
  (->> (slurp "resources/day6.txt")
       (str/split-lines)
       (mapv (comp vec seq))))

(def width (count (first input)))
(def height (count input))

(def locations (set (for [y (range height) x (range width)] [y x])))

(def obstacles (reduce (fn [acc loc]
                         (cond-> acc
                           (= \# (get-in input loc)) (conj loc)))
                       #{}
                       locations))

;; Note: using cond-> in this context is equivalent to this:
;; (reduce (fn [acc loc]
;;           (if (= \# (get-in input loc))
;;             (conj acc loc)
;;             acc))
;;         #{}
;;         locations)

(def start-pos (first (filter #(= \^ (get-in input %)) locations)))

(def up [-1 0])
(defn turn-right [[y x]] [x (- y)])

(defn mover
  [obstacles]
  (fn [[[y x :as current-pos]
        [dy dx  :as dir]]]
    (let [next-pos [(+ y dy) (+ x dx)]]
      (if (obstacles next-pos)
        [current-pos (turn-right dir)]
        [next-pos dir]))))

(def visited-pos (->> (iterate (mover obstacles) [start-pos up])
                      (take-while (comp locations first))))

;; part 1
(time
 (->> visited-pos
      (map first)
      distinct
      count))

;; part 2
(defn would-make-loop?
  ([obstacle] (would-make-loop? [start-pos up] #{} obstacle))
  ([current-pos-dir seen obstacle]
   (let [move (mover (conj obstacles obstacle))]
     (loop [current-pos-dir current-pos-dir
            seen? seen]
       (let [next-pos-dir (move current-pos-dir)]
         (cond
           (not (locations (first next-pos-dir))) false
           (seen? next-pos-dir) obstacle
           :else (recur next-pos-dir
                        (conj seen? next-pos-dir))))))))

(time (->> (disj (set (map first visited-pos)) start-pos)
           (map would-make-loop?)
           (filter identity)
           (count)))
