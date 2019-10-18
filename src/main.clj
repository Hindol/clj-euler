(ns main
  (:refer-clojure :exclude [any?])
  (:require [clj-euler.prime :as prime]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.data.int-map :as i]
            [clojure.data.csv :as csv]
            [taoensso.tufte :as tufte :refer [defnp]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(tufte/add-basic-println-handler! {})

(defn gcd
  ([x y & more]
   (reduce gcd (concat [x y] more)))
  ([x y]
   (if (> x y)
     (if (zero? y)
       x
       (recur y (rem x y)))
     (recur y x))))

(defn- digit-count
  "Given a number and optionally its exponent, outputs the number of digits"
  ([x] (inc (long (Math/floor (Math/log10 x)))))
  ([x p]
   (inc (long (Math/floor (* p (Math/log10 x)))))))

(defn- digits
  [x]
  (into []
        (reverse
         (loop [x x
                ds (transient [])]
           (if (pos? x)
             (recur (quot x 10) (conj! ds (rem x 10)))
             (persistent! ds))))))

(defn- differences
  [xs]
  (->> xs
       (partition 2 1)
       (map (fn [[m n]] (- m n)))))

(def any? (complement not-any?))

(def ^:private e
  "e as an infinite continued fraction [2;1,2,1,1,4,1,1,6,1,...,1,2k,1,...])"
  (->> (iterate (partial + 2) 4)
       (mapcat (fn [x] (list 1 1 x)))
       (concat '(2 1 2))))

(defn convergents
  "From an infinite continued fractions, find the convergents"
  [cf]
  (let [step (fn step [x y [c & more]]
               (lazy-seq
                (let [z (+' (*' c y) x)]
                  (cons z (step y z more)))))]
    (map vector (step 0 1 cf) (step 1 0 cf))))

(defn- factors
  [x]
  (mapcat (fn [m] (list m (/ x m)))
          (filter #(zero? (rem x %))
                  (range 1 (inc (Math/floor (math/sqrt x)))))))

(defn- times-divisible
  "How many times can d evenly divide x?"
  ^long [^long x ^long d]
  (loop [x x
         t 0]
    (if (zero? (rem x d))
      (recur (quot x d) (inc t))
      t)))

(defn- count-divisors
  [x]
  (->> x
       prime/factorize
       (map #(->> % second inc))
       (reduce *)))

(def ^:private factorial
  (memoize
   (fn [x]
     (if (#{0 1} x)
       1
       (* x (factorial (dec x)))))))

(defn- concat-digits
  [x y]
  (let [dcy (digit-count y)
        m (long (Math/pow 10 dcy))]
    (+ y (* m x))))

(defn- consecutive?
  [xs]
  (->> xs
       differences
       (#(or (every? #{-1} %)
             (every? #{1} %)))))

(defn polygonal-numbers
  [n]
  (map (condp = n
         3 #(/ (* % (inc %)) 2)
         4 #(* % %)
         5 #(/ (* % (dec (* 3 %))) 2)
         6 #(* % (dec (* 2 %)))
         7 #(/ (* % (- (* 5 %) 3)) 2)
         8 #(* % (- (* 3 %) 2)))
       (iterate inc 1)))

(defn filter-range
  [start end xs]
  (take-while #(< % end)
              (drop-while #(< % start) xs)))

(defn take-distinct
  [xs]
  (loop [[x & more] xs
         seen       (transient #{})
         collect    (transient [])]
    (if (seen x)
      (persistent! collect)
      (recur more (conj! seen x) (conj! collect x)))))

(defn continued-fraction
  [& {:keys [square-root]}]
  (let [root (long (Math/floor (Math/sqrt square-root)))
        step (fn [[digit {:keys [numerator denominator]}]]
               (let [i (->> denominator :subtract second)
                     j (/ (- square-root (* i i)) numerator)
                     k (quot (+ root i) j)]
                 [k
                  {:numerator   j
                   :denominator {:subtract [{:square-root square-root} (- (* k j) i)]}}]))]
    ((juxt first rest)
         (map first
              (take-distinct
               (iterate step [root {:numerator 1 :denominator {:subtract [{:square-root square-root} root]}}]))))))

(defn square?
  [x]
  (if (pos? x)
    (let [root (Math/round (Math/sqrt x))]
      (= (* root root) x))
    false))

(defn pell-find-x
  [d]
  {:pre [(not (square? d))]}
  (let [cf (continued-fraction :square-root d)]
    (ffirst
     (drop-while (fn [[x y]] (not= 1 (- (*' x x) (*' d y y)))) (convergents (cons (first cf) (cycle (second cf))))))))

(defn phi
  "Euler's totient of a number"
  [x]
  (let [prime-factors (prime/factorize x)
        primes        (keys prime-factors)
        powers        (vals prime-factors)]
    (reduce (fn [p q]
              (- p (/ p q)))
            x
            primes)))

(defn multiples
  [xs]
  (for [k (iterate inc 1)]
    (mapv (partial * k) xs)))

(defn int-seq
  [s]
  (map read-string (re-seq #"\d+" s)))

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

(defn square-root
  [x precision]
  (letfn [(step [[a b precision]]
            (if (>= a b)
              [(-' a b) (+' 10 b) precision]
              [(*' 100 a) (+' 5 (*' 10 (-' b 5))) (inc precision)]))]
    (#(-> % first second (quot 100))
     (drop-while (fn [[a b p]] (< p precision))
                 (iterate step [(* 5 x) 5N 0])))))

(defn char->digit
  ^long [^Character ch]
  (Character/digit ch 10))

(defn choose
  [n r]
  (if (> n (* 2 r))
    (/ (reduce *' (take r (iterate dec n)))
       (reduce *' (range 2 (inc r))))
    (choose n (- n r))))
