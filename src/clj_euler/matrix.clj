(ns clj-euler.matrix)

(defn read-matrix!
  [rdr]
  (mapv #(into [] (int-seq %)) (line-seq rdr)))

(def matrix-sum
  (memoize
   (fn
     ([matrix]
      (let [row-count      (count matrix)
            column-count   (count (first matrix))
            column-choices (into (sorted-set) (range column-count))]
        (matrix-sum matrix 0 column-choices)))
     ([matrix row column-choices]
      (if (empty? column-choices)
        0
        (apply max
               (for [column column-choices]
                 (+ (get-in matrix [row column])
                    (matrix-sum matrix (inc row) (disj column-choices column))))))))))
