(ns aoc2024.day6
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.matrix :as mat]))

(def test-input-str
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")
(def input-str (slurp "resources/day6.txt"))

(defn index-input
  [s]
  (->> s
       (str/split-lines)
       (map #(str/split % #""))
       (map #(map-indexed (fn [i v] [i v]) %))
       (map-indexed (fn [i v] [i v]))))

(def test-input (index-input test-input-str))
(def input (index-input input-str))

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

(defn is-obstacle?
  [pos]
  (when pos
    (= "#" (nth pos 2))))

()

(defn get-visisted
  [input]
  (let [init-pos (get-init-pos input)]
    (loop [dir 0
           current-pos init-pos
           visited #{current-pos}]
      (if-let [next-pos (get-next-pos current-pos dir input)]
        (if (is-obstacle? next-pos)
          (recur (next-dir dir)
                 current-pos
                 visited)
          (recur dir
                 next-pos
                 (set/union #{current-pos} visited)))
        (set/union #{current-pos} visited)))))

(def visited input)

(time (count visited))

;; Failed and incomplete try for Part 2
(defn exists-obstacle-up?
  [pos obstacles]
  (some #(and (< (first %) (first pos))
              (= (second %) (second pos)))
        obstacles))
(defn exists-obstacle-down?
  [pos obstacles]
  (some #(and (> (first %) (first pos))
              (= (second %) (second pos)))
        obstacles))
(defn exists-obstacle-left?
  [pos obstacles]
  (some #(and (= (first %) (first pos))
              (< (second %) (second pos)))
        obstacles))
(defn exists-obstacle-right?
  [pos obstacles]
  (some #(and (= (first %) (first pos))
              (> (second %) (second pos)))
        obstacles))

(defn is-next-a-valid-option?
  [current-pos obstacles next-dir]
  (cond (= 0 next-dir) (exists-obstacle-up? current-pos obstacles)
        (= 1 next-dir) (exists-obstacle-right? current-pos obstacles)
        (= 2 next-dir) (exists-obstacle-down? current-pos obstacles)
        (= 3 next-dir) (exists-obstacle-left? current-pos obstacles)))

(defn str-grid->vec
  [grid]
  (->> grid
       (str/split-lines)
       (map #(str/split % #""))
       (vec)))

(defn vec->str-grid
  [vectors]
  (->> vectors
       (map #(reduce str %))
       (str/join "\n")))

(mat/transpose (str-grid->vec test-input-str))

(defn replace-point
  "Input is an vector of vectors representing a 2D matrix.
  Point should have format [y x val]"
  [new-point input]
  (assoc input
         (first new-point)
         (assoc (nth input (first new-point))
                (second new-point)
                (nth new-point 2))))

(defn replace-points
  [new-points input]
  (reduce (fn [acc new-point]
            (replace-point new-point acc))
          input
          new-points))

(defn print-input-with-options
  [options input-str]
  (println (str "\n"
                (->> (str-grid->vec input-str)
                     (replace-points options)
                     (vec->str-grid)))))

;; parse every obstacles
;; then check if adding an obstacle would make a rectangle
(defn parse-obstacles-down
  "Input is a 2D matrix as vectors"
  [input]
  (reduce (fn [omap row]
            (assoc omap
                   (first row)
                   (-> (reduce (fn [acc e]
                                 (if (= "#" (second e))
                                   (set/union #{(first e)} acc)
                                   acc))
                               #{}
                               (second row))
                       (sort))))
          {}
          input))

(defn parse-obstacles-up
  "Input is a 2D matrix as vectors"
  [input]
  (reduce (fn [omap row]
            (assoc omap
                   (first row)
                   (-> (reduce (fn [acc e]
                                 (if (= "#" (second e))
                                   (set/union #{(first e)} acc)
                                   acc))
                               #{}
                               (second row))
                       (sort)
                       (reverse))))
          {}
          input))

(defn parse-obstacles-right
  "Input is a 2D matrix as vectors"
  [input]
  (reduce (fn [omap row]
            (assoc omap
                   (first row)
                   (-> (reduce (fn [acc e]
                                 (if (= "#" (second e))
                                   (set/union #{(first e)} acc)
                                   acc))
                               #{}
                               (second row))
                       (sort)
                       (reverse))))
          {}
          input))


(defn exists-loop?
  [obstacles tested-obstacle dir]
  ())

(defn count-infinite-loops
  [input]
  (let [init-pos (get-init-pos input)]
    (loop [dir 0
           current-pos init-pos
           obstacles #{}
           options #{}]
      (if-let [next-pos (get-next-pos current-pos dir input)]
        (if (is-obstacle? next-pos)
          (recur (next-dir dir)
                 current-pos
                 (set/union #{next-pos} obstacles)
                 options)
          (recur dir
                 next-pos
                 obstacles
                 (if (and (is-next-a-valid-option? current-pos
                                                   obstacles
                                                   (next-dir dir))
                          (not= next-pos init-pos))
                   (do
                     (println (str "Option: " next-pos " Obstacles: " obstacles))
                     (set/union #{(assoc next-pos 2 "0")} options))
                   options)))
        (count options)))))

;(println (str "\n" (count-infinite-loops input)))
