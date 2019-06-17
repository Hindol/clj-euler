(ns clj-euler.prime
  (:import [java.util BitSet]))

;; certainty of 15 is sufficient till 2^32
(def ^:private ^:const certainty 5)

(def probable-prime?
  (fn
    [n]
    (.isProbablePrime (BigInteger/valueOf n) certainty)))

(def prime-seq
  "Lazy sequence of all the prime numbers."
  (concat
   [2 3 5 7]
   (lazy-seq
    (let [primes-from (fn primes-from [n [f & r]]
                        (if (some #(zero? (rem n %))
                                  (take-while #(<= (* % %) n) prime-seq))
                          (recur (+ n f) r)
                          (lazy-seq (cons n (primes-from (+ n f) r)))))
          wheel       (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                              6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                              2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))

(defn sieve
  "Efficiently return all primes till end (exclusive)"
  [end]
  (let [non-primes  (BitSet. end)
        upper-limit (Math/floor (Math/sqrt end))]
    (doseq [x [0 1]]
      (.set non-primes ^long x true))
    (doseq [x     (range 2 (inc upper-limit))
            :when (not (.get non-primes x))]
      (doseq [y (range (+ x x) end x)]
        (.set non-primes ^long y true)))
    (remove #(.get non-primes %) (range 2 end))))

(defn prime?
  [x]
  (every? #(pos? (rem x %))
          (take-while #(<= (* % %) x) prime-seq)))

(def factorize
  (memoize
   (fn [x]
     (if (< x 2)
       {}
       (let [prime-factor (first (filter #(zero? (rem x %)) (take-while #(<= % x) prime-seq)))
             quotient     (quot x prime-factor)]
         (cond->> {prime-factor 1}
           (> quotient 1) (merge-with + (factorize quotient))))))))
