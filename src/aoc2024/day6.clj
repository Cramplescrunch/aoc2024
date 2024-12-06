(ns aoc2024.day6
  (:require [clojure.string :as str]))

(def test-input
  (->> "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."
      (str/split-lines)
      (map #(str/split % #""))
      (map #(map-indexed (fn [i v] [i v]) %))
      (map-indexed (fn [i v] [i v]))))

(def input
  (->> (slurp "resources/day6.txt")
       (str/split-lines)
       (map #(str/split % #""))
       (map #(map-indexed (fn [i v] [i v]) %))
       (map-indexed (fn [i v] [i v]))))

;; 0 = up, 1 = right, 2 = down, 3 = left (to use mod to compute next dir)
(defn next-dir
  [current-dir]
  (mod (+ 1 current-dir) 4))

(defn find-init-in-row
  [row]
  (let [found (reduce (fn [v1 v2]
                        (cond (= "^" (second v1)) (reduced v1)
                              (= "^" (second v2)) (reduced v2)
                              :else v2))
                      row)]
    (if (= "^" (second found))
      found
      nil)))

(defn get-init-pos
  [input]
  (->> input
       (reduce (fn [row1 row2]
                 (let [found1 (find-init-in-row (second row1))
                       found2 (find-init-in-row (second row2))]
                   (cond found1 (reduced (cons (first row1) found1))
                         found2 (reduced (cons (first row2) found2))
                         :else  row2))))))

(get-init-pos test-input)

(defn get-pos-value
  [input x y]
  (try
    (-> (nth input y)
        (second)
        (nth x)
        (second))
    (catch Exception e nil)))

;; a pos is defined like (y x val)
(defn get-pos
  [input y x]
  (if-let [val (get-pos-value input x y)]
    [y x val]
    nil))

(defn get-next-pos
  [current-pos dir input]
  (cond (= 0 dir) (get-pos input (dec (first current-pos)) (second current-pos))
        (= 1 dir) (get-pos input (first current-pos) (inc (second current-pos)))
        (= 2 dir) (get-pos input (inc (first current-pos)) (second current-pos))
        (= 3 dir) (get-pos input (first current-pos) (dec (second current-pos)))))

(defn count-pos
  [input]
  (let [init-pos (get-init-pos input)]
    (loop [dir 0
           current-pos init-pos
           next-pos (get-next-pos current-pos dir input)
           count 0]
      (if next-pos
        (let [dir (if (= "#" (nth next-pos 2))
                    (next-dir dir)
                    dir)]
          (recur dir
                 next-pos
                 (get-next-pos next-pos dir input)
                 (inc count)))
        (+ 1 count)))))

(count-pos test-input)
