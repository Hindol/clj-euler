(ns clj-euler.geometry)

(defn point-in-triangle
  [{:keys [p q r] :as triangle} s]
  (let [alpha (/ (+ (* (- (:y q) (:y r)) (- (:x s) (:x r)))
                    (* (- (:x r) (:x q)) (- (:y s) (:y r))))
                 (+ (* (- (:y q) (:y r)) (- (:x p) (:x r)))
                    (* (- (:x r) (:x q)) (- (:y p) (:y r)))))
        beta  (/ (+ (* (- (:y r) (:y p)) (- (:x s) (:x r)))
                    (* (- (:x p) (:x r)) (- (:y s) (:y r))))
                 (+ (* (- (:y q) (:y r)) (- (:x p) (:x r)))
                    (* (- (:x r) (:x q)) (- (:y p) (:y r)))))
        gamma (- 1.0 alpha beta)]
    (every? pos? [alpha beta gamma])))
