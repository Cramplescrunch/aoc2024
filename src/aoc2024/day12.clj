(ns aoc2024.day12
  (:require [clojure.string :as str]))

;; Lots of experimental code before the answers but keeping it for later uses
(def input
  (->> (slurp "resources/day12.txt")
       (str/split-lines)
       (mapv (comp vec seq))))
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
  [pos plant]
  (->> (get-adjacent-pos pos)
       (filter (fn [p] (= plant (get-pos-value p))))))

(defn scout-area
  "Returns a set of all the locations of the area around starting point"
  [start-pos]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start-pos)
         visited #{}]
    (cond
      (empty? queue)
      visited

      (visited (peek queue))
      (recur (pop queue) visited)

      :else
      (let [current (peek queue)
            value (get-pos-value current)
            valid-adjacent-nodes (get-valid-adjacent-nodes current value)
            queue (into (pop queue) valid-adjacent-nodes)
            visited (conj visited current)]
        (recur queue visited)))))

(def first-area (scout-area [0 0]))

(defn is-outside?
  [area pos]
  (not (area pos)))

(def up [-1 0])
(defn turn-left [[y x]] [(- x) y])
(defn turn-right [[y x]] [x (- y)])

(defn get-right-pos
  [[[y x :as pos] dir]]
  (let [[dy dx] (turn-right dir)]
   [[(+ y dy) (+ x dx)] dir]))

(defn get-right-fence
  [area pos-dir]
  (let [[right-pos _ :as right-pos-dir] (get-right-pos pos-dir)]
    (when (is-outside? area right-pos) right-pos-dir)))

(defn move
  [area
   [[y x :as current-pos]
    [dy dx  :as dir] :as pos-dir]]
  (let [next-pos [(+ y dy) (+ x dx)]]
    (cond
      (and (is-outside? area next-pos)
           (get-right-fence area pos-dir))
      [current-pos (turn-left dir)]

      (get-right-fence area pos-dir)
      [next-pos dir]

      :else
      (let [[dry drx :as right-dir] (turn-right dir)
            next-right-pos [(+ y dry) (+ x drx)]]
        [next-right-pos right-dir]))))

(defn get-min-x
  [area min-y]
  (->> area
     (filter #(= min-y (first %)))
     (mapv second)
     (apply min)))

(defn get-start-pos-with-dir
  [area]
  (let [min-y (first (apply min-key first area))]
   [[min-y (get-min-x area min-y)] [0 -1]]))

(def condj ((remove nil?) conj))

(defn count-fences
  [area]
  (if (= 1 (count area))
    4
    (let [start-pos-with-dir (get-start-pos-with-dir area)]
      (loop [pos-with-dir start-pos-with-dir
             fences (condj #{} (get-right-fence area pos-with-dir))
             visited #{}]
        ;(println (str "pos: " pos-with-dir " count: " (count fences)))
        (if (and (> (count visited) 1)
                 (= pos-with-dir start-pos-with-dir))
          (do
            ;(println fences)
            (count (condj fences (get-right-fence area pos-with-dir))))
          (let [next-pos-with-dir (move area pos-with-dir)]
            (recur next-pos-with-dir
                   (condj fences (get-right-fence area pos-with-dir))
                   (conj visited pos-with-dir))))))))

(def areas
  (first (reduce (fn [[areas visited :as acc] location]
             (if (visited location)
               acc
               (let [area (scout-area location)]
                 [(conj areas area) (into visited area)])))
           [#{} #{}]
           locations)))

;; Part 1
;; Wrong because I need to count inner fences as well
(->> areas
     (map (fn [area] (* (count area) (count-fences area))))
     (reduce +))

;; part 1 using BFS
(defn scout-area-and-perimeter
  [start-pos]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start-pos)
         visited #{}
         fences-count 0]
    (cond
      (empty? queue)
      [visited fences-count]

      (visited (peek queue))
      (recur (pop queue) visited fences-count)

      :else
      (let [current (peek queue)
            value (get-pos-value current)
            valid-adjacent-nodes (get-valid-adjacent-nodes current value)
            queue (into (pop queue) valid-adjacent-nodes)
            counter (- 4 (count valid-adjacent-nodes))
            visited (conj visited current)]
        (recur queue visited (+ fences-count counter))))))

(def areas-and-perimeters
  (first (reduce (fn [[areas visited :as acc] location]
                   (if (visited location)
                     acc
                     (let [area-and-perim (scout-area-and-perimeter location)]
                       [(conj areas area-and-perim) (into visited (first area-and-perim))])))
                 [#{} #{}]
                 locations)))

(time (->> areas-and-perimeters
           (map (fn [[area perimeter]] (* (count area) perimeter)))
           (reduce +)))

;; Part 2: count corners
(defn has-corner?
  [area [[y x] [dy dx :as dir]]]
  (let [up-pos [(+ y dy) (+ x dx)]
        [ry rx :as right-dir] (turn-right dir)
        right-pos [(+ y ry) (+ x rx)]
        diag-right-pos [(+ y dy ry) (+ x dx rx)]]
    ;; Has corner if:
    ;; - up, right and diag-right are outside
    ;; - up and right are in area, diag-right is outside
    ;; - up and right are outside, diag-right is inside
    (or (and (is-outside? area up-pos)
             (is-outside? area right-pos)
             (is-outside? area diag-right-pos))
        (and (is-outside? area diag-right-pos)
             (not (is-outside? area up-pos))
             (not (is-outside? area right-pos)))
        (and (not (is-outside? area diag-right-pos))
             (is-outside? area up-pos)
             (is-outside? area right-pos)))))

(def cardinals (take 4 (iterate turn-right up)))

(defn count-corners
  [area pos]
  (->> (map (fn [card] (has-corner? area [pos card])) cardinals)
       (filter true?)
       count))

(defn count-area-corners
  [area]
  (->> (map #(count-corners area %) area)
       (reduce +)))

(def areas-and-sides
  (map (fn [area] [area (count-area-corners area)]) areas))

(time (->> areas-and-sides
           (map (fn [[area sides]] (* (count area) sides)))
           (reduce +)))
