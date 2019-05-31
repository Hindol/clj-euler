(ns clj-euler.prime)

;; certainty of 15 is sufficient till 2^32
(def ^:private ^:const certainty 5)

(def probable-prime?
  (fn
    [n]
    (.isProbablePrime (BigInteger/valueOf n) certainty)))

(def primes
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

(defn prime?
  [x]
  (every? #(pos? (rem x %))
          (take-while #(<= (* % %) x) primes)))

#_(defn prime-factorize
  [x]
  (frequencies (loop [x                         x
                      [c & more :as candidates] (take-while #(<= % x) primes)
                      prime-factors             (transient [])]
                 (if (> x 1)
                   (if (zero? (rem x c))
                     (recur (quot x c) candidates (conj! prime-factors c))
                     (recur x more prime-factors))
                   (persistent! prime-factors)))))

(def factorize
  (memoize
   (fn [x]
     (if (< x 2)
       {}
       (let [prime-factor (first (filter #(zero? (rem x %)) (take-while #(<= % x) primes)))
             quotient     (quot x prime-factor)]
         (cond->> {prime-factor 1}
           (> quotient 1) (merge-with + (factorize quotient))))))))
