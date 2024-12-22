(ns aoc2024.day13
  (:require [clojure.string :as str]))

(defrecord Point [x y])
(defrecord Machine [A B prize])
(defn str->Point
  [s]
  (->> (rest (re-find #"X.(\d+), Y.(\d+)" s))
       (map parse-long)
       (apply ->Point)))
(defn strs->machine
  [[a b prize]]
  (Machine. (str->Point a)
            (str->Point b)
            (str->Point prize)))

(def input
  (let [splitted
        (-> (slurp "resources/day13.txt")
            (str/split #"\n\n"))]
    (map (comp strs->machine str/split-lines) splitted)))

;; First draft for part 1 (trying to solve without linear equation methods)
;; Spoiler: doesn't work
(defn find-valid-x-muls
  "Returns a set of [mul-x-max, mul-x-min] pairs with multiplicators leading to target"
  [xa xb target]
  (let [x-max (max xa xb)
        x-min (min xa xb)
        lo (* 100 x-min)]
    (cond
      (< target (* 100 x-min))
      #{} ; if target is the minimum it means there is no value with which to reach it

      (= target (* 100 x-min))
      #{[0 100]}

      :else
      (set (for [mul (range 100)
                 :let [res (* x-max mul)
                       remaining (- target res)
                       x-min-mul (/ remaining x-min)]
                 :when (and (<= lo res target)
                            (= 0 (mod remaining x-min))
                            (<= x-min-mul 100))]
             (if (= xa x-max)
               [mul (/ remaining x-min)]
               [(/ remaining x-min) mul]))))))

(defn find-valid-y-muls
  [ya yb target valid-x-muls]
  (set (filter (fn [[mxa mxb]] (= target (+ (* ya mxa) (* yb mxb)))) valid-x-muls)))

(defn get-cost
  [[ma mb]]
  (+ (* 3 ma) mb))

(defn find-optimal-move
  [{{xa :x ya :y} :A
    {xb :x yb :y} :B
    {px :x py :y} :prize}]
  (->> (find-valid-x-muls xa xb px)
       (find-valid-y-muls ya yb py)
       (sort (fn [e1 e2] (compare (get-cost e1) (get-cost e2))))
       first))

;(time
; (->> input
;      (map find-optimal-move)
;      (filter identity)
;      (map get-cost)
;      (reduce +)))

;; Try solving with linear equation methods

(defn find-solutions
  [{{xa :x ya :y} :A
    {xb :x yb :y} :B
    {px :x py :y} :prize}]
  (let [det-A (- (* xa yb) (* xb ya))]
    (if (zero? det-A)
      nil ; either zero solutions or an infinity of solutions (inconsistent)
      (let [det-Ax (- (* px yb) (* xb py))
            det-Ay (- (* xa py) (* px ya))
            solution [(/ det-Ax det-A) (/ det-Ay det-A)]]
        (if (and (= java.lang.Long (type (first solution)))
                 (= java.lang.Long (type (second solution))))
          solution
          nil)))))

(find-solutions (nth input 2))

(time (->> input
           (map find-solutions)
           (filter identity)
           (map get-cost)
           (reduce +)))

;; part 2
(defn correct-coordinates
  [{A :A
    B :B
    {px :x py :y} :prize}]
  (Machine. A B (Point. (+ 10000000000000 px) (+ 10000000000000 py))))

(time (->> input
           (map (comp find-solutions correct-coordinates))
           (filter identity)
           (map get-cost)
           (reduce +)))
