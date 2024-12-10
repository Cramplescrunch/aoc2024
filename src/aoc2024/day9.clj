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
       (filter identity)
       (map-indexed (fn [i v] [i v []]))
       vec))
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

(defn find-first
  [pred coll]
  (let [found (reduce (fn [e1 e2]
                        (cond
                          (pred e1) (reduced e1)
                          (pred e2) (reduced e2)
                          :else e2))
                      coll)]
    (if (pred found)
      found
      nil)))

(defn find-place-for-file
  "Returns freespaces updated with file if there's room, or freespaces otherwise"
  [freespaces file]
  (let [get-free-space (fn [space] (second space))
        get-idx (fn [space] (first space))
        get-files (fn [space] (nth space 2))]
    (if-let [found (find-first #(>= (get-free-space %) (count file)) freespaces)]
      (assoc freespaces (get-idx found) [(get-idx found)
                                         (- (get-free-space found) (count file))
                                         (into (get-files found) [file])])
      freespaces)))

(defn space->files
  [space]
  (into (nth space 2) (vec (repeatedly (second space) (fn [] (char \.))))))

(def files-to-fill
  (->> (reduce (fn [acc file]
                 (find-place-for-file acc file))
               freespaces
               (reverse -files))
       (reduce (fn [acc space]
                 (conj acc (space->files space)))
               [])))
(def moved-files
  (->> files-to-fill
       flatten
       (filter #(not= \. %))
       set))

(->> (flatten (map(fn [f1 f2]
                    (if (moved-files f1)
                      [(repeatedly (count f1) (fn [] (char \.))) f2]
                      [f1 f2]))
                  -files
                  files-to-fill))
     (reduce (fn [acc file]
               (if (= String (type file))
                 (into acc  (vec (seq (chars (char-array file)))))
                 (conj acc file)))
             [])
     (map-indexed (fn [i c]
                    (if (not= \. c)
                      (* i (Character/digit c 10))
                      0)))
     (reduce +))
