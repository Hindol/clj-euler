(ns main
  (:refer-clojure :exclude [any?])
  (:require [clj-euler.prime :as prime]
            [clj-java-decompiler.core :refer [decompile]]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [criterium.core :refer :all]
            [taoensso.tufte :as tufte :refer [defnp]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(tufte/add-basic-println-handler! {})

(defn- nth-root
  [x n]
  (Math/pow x (/ 1.0 n)))

(defn- gcd
  [x y]
  (if (> x y)
    (if (zero? y)
      x
      (recur y (rem x y)))
    (recur y x)))

(defn- digit-count
  "Given a number and optionally its exponent, outputs the number of digits"
  ([x] (inc (long (Math/floor (Math/log10 x)))))
  ([x p]
   (inc (long (Math/floor (* p (Math/log10 x)))))))

(defn- digits
  [x]
  (reverse
   (loop [x x
          ds []]
     (if (pos? x)
       (recur (quot x 10) (conj ds (rem x 10)))
       ds))))

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

(defn- nth-convergent
  "Given an infite continued fraction, finds the nth convergent"
  [xs n]
  (let [f (first xs)
        r (rest xs)]
    (if (zero? n)
      f
      (+ f (/ (nth-convergent r (dec n)))))))

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

(comment
  (reduce #(rem (*' %1 %2) 500500507)
          (take 500500
                (let [step (fn step
                             ([ps]
                              (step ps (sorted-set (first ps))))
                             ([[p q & more :as ps] candidates]
                              (lazy-seq
                               (let [f (first candidates)
                                     cs (-> candidates (disj f) (conj (* f f)))]
                                 (if (= f p)
                                   (cons f (step (cons q more) (conj cs q)))
                                   (cons f (step ps cs)))))))]
                  primes))))

(defonce ^:private factorial
  (memoize
   (fn [x]
     (if (#{0 1} x)
       1
       (* x (factorial (dec x)))))))

(defn- sum-till
  [n]
  (quot (* n (inc n)) 2))

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

(def count-in-factorial
  (memoize
   (fn [p x]
     (let [x (- x (rem x p))]
       (if (< x p)
         0
         (+ (times-divisible x p) (count-in-factorial p (- x p))))))))

(defn kempner
  ([p e]
   (* p (inc (count (take-while #(< % e) (map #(count-in-factorial p %) (iterate #(+ p %) p)))))))

  ([x]
   (apply max (map #(apply kempner %) (prime/factorize x)))))

(reduce +' (map kempner (range 2 650001)))
