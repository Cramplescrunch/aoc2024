(ns aoc2024.day7
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def input (->> (slurp "resources/day7.txt")
                (str/split-lines)
                (mapv #(str/split % #"\: | "))
                (mapv (fn [v] (mapv #(Long/parseLong %) v)))))

(defn get-combinations
  [dimension operators]
  (combo/selections operators dimension))

(defn solves?
    [target operands operators]
  (let [mapped-operands (map vector operators (rest operands))]
    (-> (reduce (fn [acc [op operand]]
                  (op acc operand))
                (first operands)
                mapped-operands)
        (= target))))

(defn has-solution?
  [operators line]
  (let [target (first line)
        operands (rest line)
        operators-comb (get-combinations (count operands) operators)]
    (reduce (fn [ops1 ops2]
              (cond (solves? target operands ops1) (reduced target)
                    (solves? target operands ops2) (reduced target)))
            operators-comb)))

;; part 1
(time (->> input
           (pmap (partial has-solution? [+ *]))
           (filter identity)
           (reduce +)))

;; part 2
(defn ||
  [x y]
  (Long/parseLong (str x y)))

(->> input
     (pmap (partial has-solution? [+ * ||]))
     (filter identity)
     (reduce +))
