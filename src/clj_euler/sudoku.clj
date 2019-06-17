(ns clj-euler.sudoku
  (:require [clojure.set :as s]))

(defn read-sudoku!
  [rdr]
  (let [lines  (->> rdr
                    line-seq
                    (drop 1)
                    (take 9))
        parsed (for [line lines]
                 (mapv #(Character/digit % 10) line))]
    (into [] parsed)))

(defn- row
  [sudoku n]
  (into (sorted-set)
        (nth sudoku n)))

(defn- column
  [sudoku n]
  (into (sorted-set)
        (map #(nth % n) sudoku)))

(defn- region
  [sudoku x y]
  (let [start-x (- x (rem x 3))
        end-x   (+ 3 start-x)
        sub-x   (subvec sudoku start-x end-x)
        start-y (- y (rem y 3))
        end-y   (+ 3 start-y)]
    (into (sorted-set)
          (mapcat #(subvec % start-y end-y) sub-x))))

(defn- next-cell
  [x y] (cond (< y 8) [x (inc y)]
    (< x 8) [(inc x) 0]))

(def ^:private ^:constant choices
  (sorted-set 1 2 3 4 5 6 7 8 9))

(defn- valid-choices
  [sudoku x y]
  (let [n (get-in sudoku [x y])]
    (if (zero? n)
      (s/difference choices (row sudoku x) (column sudoku y) (region sudoku x y))
      (sorted-set n))))

(defn solve-sudoku
  ([sudoku]
   (solve-sudoku sudoku 0 0))
  ([sudoku x y]
   (let [chcs (valid-choices sudoku x y)]
     (if-let [[next-x next-y] (next-cell x y)]
       (first (remove nil?
                      (for [n chcs]
                        (if-let [solved (solve-sudoku (assoc-in sudoku [x y] n) next-x next-y)]
                          solved))))
       (if (seq chcs)
         (assoc-in sudoku [x y] (first chcs)))))))
