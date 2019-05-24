(ns main
  (:refer-clojure :exclude [any?])
  (:require [clj-java-decompiler.core :refer [decompile]]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]
            [criterium.core :refer :all]))

;; certainty of 15 is sufficient till 2^32
(def ^:private ^:const certainty 5)

(defonce probable-prime?
  (memoize
   (fn
     [n]
     (.isProbablePrime (BigInteger/valueOf n) certainty))))

(def ^:private primes
  "Lazy sequence of all the prime numbers."
  (concat
   [2 3 5 7]
   (lazy-seq
    (let [primes-from (fn primes-from [n [f & r]]
                        (if (some #(zero? (rem n %))
                                  (take-while #(<= (* % %) n) primes))
                          (recur (+ n f) r)
                          (lazy-seq (cons n (primes-from (+ n f) r)))))
          wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                        6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                        2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))

(defn- prime?
  [x]
  (every? #(pos? (rem x %))
          (take-while #(<= (* % %) x) primes)))

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
  "How many times can d evenly divide x? Returns the count and the remainder."
  [x d]
  (loop [x x
         t 0]
    (if (zero? (rem x d))
      (recur (quot x d) (inc t))
      [t x])))

(defn- ^:private prime-factorize
  "Returns a lazy sequence prime factors (and their exponents)"
  [x]
  (let [step (fn f [x [p & more]]
               (if (<= p x)
                 (let [[t r] (times-divisible x p)]
                   (cons [p t] (f r more)))
                 '()))]
    (remove #(->> % second zero?) (step x primes))))

(defn- count-divisors
  [x]
  (->> x
       prime-factorize
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

(defn- read-matrix
  [path]
  (let [m      (->> path
                    io/resource
                    io/reader
                    line-seq
                    (mapv #(mapv read-string (str/split % #","))))
        height (count m)
        width  (count (first m))]
    {:elements m
     :width    width
     :height   height}))

(defn- find-shortest-distance
  [{:keys [elements width height]} [sx sy] [ex ey] neighbor-fn]
  (let [index-in-bounds? (fn [[x y]]
                           (and (< -1 x height)
                                (< -1 y width)))
        visited          (atom #{})
        distances        (atom {[sx sy] (get-in elements [sx sy])})]
    (loop [[x y] [sx sy]]
      (let [neighbors (filter index-in-bounds?
                              (neighbor-fn [x y]))]
        (if (= [x y] [ex ey])
          ;; If we are at destination, return
          (get @distances [ex ey])
          ;; Else, maybe update all neighbours distances
          (do
            (doseq [nbr neighbors]
              (swap! distances
                     assoc
                     nbr
                     (min (get @distances nbr Long/MAX_VALUE)
                          (+ (get @distances [x y])
                             (get-in elements nbr)))))
            (swap! visited
                   conj
                   [x y])
            (let [nxt (first (apply min-key
                                    second
                                    (remove #(contains? @visited (first %))
                                            (seq @distances))))]
              (recur nxt))))))))

(defn- four-neighbors
  [[x y]]
  [[(dec x) y] [x (inc y)] [(inc x) y] [x (dec y)]])

(def ^:private ^:constant sample
  {:elements [[131 673 234 103 18]
              [201 96 342 965 150]
              [630 803 746 422 111]
              [537 699 497 121 956]
              [805 732 524 37 331]]
   :height   5
   :width    5})

(find-shortest-distance #_sample (read-matrix "p083_matrix.txt") [0 0] #_[4 4] [79 79] four-neighbors)
