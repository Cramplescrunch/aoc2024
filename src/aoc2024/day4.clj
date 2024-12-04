(ns aoc2024.day4
  (:require [clojure.string :as str]))

(def input
  (-> (slurp "resources/day4.txt")
      (str/split-lines)))

(defn count-line
  [s]
  (+ (count (re-seq #"XMAS" s))
     (count (re-seq #"SAMX" s))))

(defn count-word
  [word]
  (if (or (= "XMAS" word)
          (= "SAMX" word))
    1
    0))

(defn count-vert
  [four-lines]
  (loop [idx 0
         count 0]
    (let [word (apply str (map #(nth % idx nil) four-lines))]
      (if (not-empty word)
        (recur (inc idx)
               (+ count
                  (count-word word)))
        count))))

(defn count-top-diag
  [four-lines]
  (loop [idx 0
         count 0]
    (let [c1 (nth (first four-lines) idx)
          c2 (nth (second four-lines) (inc idx))
          c3 (nth (nth four-lines 2) (+ idx 2))
          c4 (nth (nth four-lines 3) (+ idx 3) nil)
          word (str c1 c2 c3 c4)]
      (if c4
        (recur (inc idx)
               (+ count
                  (count-word word)))
        count))))

(defn count-bottom-diag
  [four-lines]
  (loop [idx 0
         count 0]
    (let [c1 (nth (first four-lines) (+ idx 3) nil)
          c2 (nth (second four-lines) (+ idx 2))
          c3 (nth (nth four-lines 2) (+ idx 1))
          c4 (nth (nth four-lines 3) idx)
          word (str c1 c2 c3 c4)]
      (if c1
        (recur (inc idx)
               (+ count
                  (count-word word)))
        count))))

(defn count-four-lines
  [line-num input]
  (let [four-lines (map #(nth input (- line-num %)) [3 2 1 0])]
    (+ (count-line (nth input line-num))
       (count-vert four-lines)
       (count-top-diag four-lines)
       (count-bottom-diag four-lines))))

(def init-count
  (+ (count-line (first input))
     (count-line (second input))
     (count-line (nth input 2))))

(defn count-rest
  [line-num input]
  (loop [line-num line-num
         count 0]
    (let [line (nth input line-num nil)]
      (if line
        (recur (inc line-num) (+ count
                                 (count-four-lines line-num input)))
        count))))

(def first-answer
  (+ init-count
     (count-rest 3 input)))

(def test-data
  (-> "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"
      (str/split-lines)))

;; Part 2
(defn count-word-p2
  [word1 word2]
  (if (and (or (= "MAS" word1)
               (= "SAM" word1))
           (or (= "MAS" word2)
               (= "SAM" word2)))
    1
    0))

(defn count-x-mas
  ([three-lines]
   (loop [idx   0
          count 0]
     (let [c1    (nth (first three-lines) idx)
           c2    (nth (nth three-lines 2) idx)
           c3    (nth (second three-lines) (+ idx 1))
           c4    (nth (first three-lines) (+ idx 2) nil)
           c5    (nth (nth three-lines 2) (+ idx 2) nil)
           word1 (str c1 c3 c5)
           word2 (str c2 c3 c4)]
       (if c4
         (recur (inc idx)
                (+ count
                   (count-word-p2 word1 word2)))
         count))))
  ([line-num input]
   (loop [line-num line-num
          count    0]
     (let [line (nth input line-num nil)]
       (if line
         (let [three-lines (map #(nth input (- line-num %)) [2 1 0])]
           (recur (inc line-num) (+ count
                                    (count-x-mas three-lines))))
         count)))))

(time (count-x-mas 2 input))
