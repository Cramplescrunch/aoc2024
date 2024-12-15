(ns aoc2024.day10
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "resources/day10.txt")
       (str/split-lines)
       (mapv (comp vec seq))
       (mapv (fn [v] (mapv #(Character/digit % 10) v)))))
(def width (count (first input)))
(def height (count input))
(def locations (set (for [y (range height) x (range width)] [y x])))

(defn get-pos-value
  [[y x :as pos]]
  (-> (nth input y)
      (nth x)))

(defn is-valid-pos?
  [[y x :as pos]]
  (and (>= x 0)
       (>= y 0)
       (< x width)
       (< y height)))

(defn get-adjacent-pos
  [[y x :as pos]]
  (into
   (for [vy [(dec y) (inc y)]
         :let [npy [vy x]]
         :when (is-valid-pos? npy)]
     npy)
   (for [vx [(dec x) (inc x)]
         :let [npx [y vx]]
         :when (is-valid-pos? npx)]
     npx)))

(defn get-valid-adjacent-nodes
  [pos height]
  (->> (get-adjacent-pos pos)
       (filter (fn [p] (= (inc height) (get-pos-value p))))))

(defn count-9s
  [queue start-pos]
  (loop [queue (conj queue start-pos)
         visited #{}
         counter 0]
    (cond
      (empty? queue)
      counter

      (visited (peek queue))
      (recur (pop queue) visited counter)

      :else
      (let [current (peek queue)
            value (get-pos-value current)
            queue (into (pop queue) (get-valid-adjacent-nodes current value))
            visited (conj visited current)
            counter (if (= 9 value) (inc counter) counter)]
        (recur queue visited counter)))))

;; Part 1
(time
 (->> locations
      (filter #(= 0 (get-pos-value %)))
      (mapv #(count-9s clojure.lang.PersistentQueue/EMPTY %))
      (reduce +)))

;; Part 2
(defn count-distinct-trails
  [pos]
  (let [height (get-pos-value pos)]
    (cond
      (= 8 height)
      (->> (get-adjacent-pos pos)
           (mapv #(if (= 9 (get-pos-value %)) 1 0))
           (reduce +))

      (<= 0 height 7)
      (let [adjacent-pos (get-adjacent-pos pos)
            valid-adj-pos (filter (fn [p] (= (inc height) (get-pos-value p))) adjacent-pos)]
        (->> (mapv count-distinct-trails valid-adj-pos)
             (reduce +)))

      :else
      (throw (ex-info "wrong height")))))

(count-distinct-trails [0 2])

(time
 (->> locations
      (filter #(= 0 (get-pos-value %)))
      (mapv #(count-distinct-trails %))
      (reduce +)))
