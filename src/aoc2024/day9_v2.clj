(ns aoc2024.day9-v2
  (:require [clojure.string :as str]
            [clojure.core.rrb-vector :as fv]))

(def test-input (->> "37177921644951938277999269384375694569563018897314838836"
                     (map #(Character/digit % 10))))

(def input (->> (slurp "resources/day9.txt")
                str/trim-newline
                (map #(Character/digit % 10))))

(defrecord File [size value moved])

(defn expand
  [disk]
  (->> disk
     (map-indexed vector)
     (reduce (fn [[acc count] el]
               (if (even? (first el))
                 [(conj acc (File. (second el) (str count) false)) (inc count)]
                 [(conj acc (File. (second el) nil false)) count]))
             [[] 0])
     first
     vec))

(def expanded-disk (expand input))
(def test-exp (expand test-input))

(defn remove-file
  "Returns disk with file replaced by empty space"
  [disk file-idx]
  (let [file (nth disk file-idx)]
    (assoc disk file-idx (File. (:size file) nil false))))

(defn insert-file
  "Returns disk with inserted file in an empty space.
  Throws exception if space non empty or not enough room"
  [disk file empty-space-idx]
  (let [empty-space (nth disk empty-space-idx)]
    (cond
      (some? (:value empty-space))
      (throw (ex-info "empty space not empty!" {}))

      (= (:size empty-space) (:size file))
      (fv/catvec (conj (fv/subvec disk 0 empty-space-idx)
                       (File. (:size file) (:value file) true))
                 (fv/subvec disk (inc empty-space-idx)))

      (> (:size empty-space) (:size file))
      (fv/catvec (conj (fv/subvec disk 0 empty-space-idx)
                       (File. (:size file) (:value file) true)
                       (File. (- (:size empty-space) (:size file)) nil false))
                 (fv/subvec disk (inc empty-space-idx))))))

(defn find-first-empty
  "Returns the index of the first empty space found"
  [disk start-idx end-idx]
  (if-let [found
           (first (keep-indexed #(when (nil? (:value %2)) %1) (fv/subvec disk start-idx end-idx)))]
    (+ start-idx found)
    nil))

(defn find-matching-empty
  "Returns index of the first empty space having size equal or superior the given size"
  [disk start-idx end-idx size]
  (if-let [found
           (first (keep-indexed #(when (and (nil? (:value %2))
                                            (>= (:size %2) size))
                                   %1)
                                (fv/subvec disk start-idx end-idx)))]
    (+ start-idx found)
    nil))

(find-first-empty expanded-disk 2 (count expanded-disk))
(find-matching-empty expanded-disk 2 (count expanded-disk) 4)

(defn defrag-disk
  [input-disk]
  (loop [disk input-disk
         i (find-first-empty disk 0 (count input-disk))
         j (dec (count disk))
         file (nth disk j)]
      (println (str i " " j " " (:value file)))
      (cond
        (nil? i)
        disk

        (> i j)
        disk

        (or (nil? (:value file))
            (true? (:moved file)))
        (recur disk i (dec j) (nth disk (dec j)))

        :else
        (if-let [ matching-empty (find-matching-empty disk i j (:size file))]
          (let [new-disk (-> (remove-file disk j)
                             (insert-file file matching-empty))]
            (recur new-disk
                   (find-first-empty new-disk i j)
                   (dec j)
                   (nth new-disk (dec j))))
          (recur disk i (dec j) (nth disk (dec j)))))))

(defn file->numseq
  [file]
  (if (nil? (:value file))
    (repeat (:size file) nil)
    (->> (repeat (:size file) (parse-long (:value file))))))

;; part 2
(time (->> (defrag-disk expanded-disk)
           (mapv file->numseq)
           flatten
           (map-indexed (fn [i v]
                          (when (some? v)
                            (* i v))))
           (filter identity)
           (reduce +)))
