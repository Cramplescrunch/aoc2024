(ns aoc2024.day2
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input-lists
  (let [lines (-> (slurp "resources/day2.txt")
                  (str/split-lines))]
    (->> lines
         (map (fn [line]
                (->> (str/split line #" ")
                     (map #(Integer/parseInt %))))))))

(defn is-safe-ordered?
  [report order-fn]
  (if (> (reduce (fn [e1 e2]
                   (if (and (order-fn e1 e2)
                            (and (>= (abs (- e1 e2)) 1)
                                 (<= (abs (- e1 e2)) 3)))
                     e2
                     (reduced 0)))
                 report) 0)
    1
    0))

(defn is-safe?
  [report]
  (cond
    (< (first report) (second report)) (is-safe-ordered? report <)
    (> (first report) (second report)) (is-safe-ordered? report >)
    :else 0))

(def xf (map is-safe?))

(def first-answer
  (transduce xf + input-lists))

;; Part 2
(defn get-order-fn
  [report]
  (if (< (first report) (second report)) < >))

(defn remove-index
  [i coll]
  (keep-indexed (fn [idx val]
                  (when (not= i idx)
                    val))
                coll))

(defn check-with-idx
  [order-fn e1-idxed e2-idxed]
  (let [e1 (second e1-idxed)
        e2 (second e2-idxed)]
    (if (and (order-fn e1 e2)
             (and (>= (abs (- e1 e2)) 1)
                  (<= (abs (- e1 e2)) 3)))
      e2-idxed
      (reduced {:failed (first e1-idxed)}))))

(defn reduce-idx
  [report]
  (let [order-fn (get-order-fn report)]
        (->> report
             (map-indexed (fn [i v] [i v]))
             (reduce (fn [e1 e2] (check-with-idx order-fn e1 e2))))))

(defn reduce-coll
  [report]
  (if-let [failed-idx (:failed (reduce-idx report))]
    (let [rep-0 (remove-index 0 report)
          rep-n (remove-index failed-idx report)
          rep-np1 (remove-index (+ 1 failed-idx) report)]
      (if (and (:failed (reduce-idx rep-0))
               (:failed (reduce-idx rep-n))
               (:failed (reduce-idx rep-np1)))
        0
        1))
    1))

(def xf2 (map reduce-coll))

(def second-answer
  (transduce xf2 + input-lists))
