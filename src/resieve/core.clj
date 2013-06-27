(ns resieve.core
  (:require [clojure.core.reducers :as r])
  (gen-class))


(defn sieve 
    ([x] (sieve [] (range 2 x)))
    ([x xs]
       (if-let [prime (first xs)]
             (recur (conj x prime) (remove #(zero? (mod % prime)) xs))
             x)))

(defn primes
    ([] (primes (iterate inc 2)))
    ([x] (cons (first x)
         (lazy-seq (primes (remove #(zero? (mod % (first x))) (rest x)))))))

(defn mod-filter [p]
  (fn [n]
    (if (nil? n) 
        n
        (if (zero? (mod n p)) n))))

resieve.core=> (defn xfilt [n]
          #_=>   (prn "x" n)
          #_=>   (zero? (mod n 3)))
#'resieve.core/xfilt
resieve.core=> (defn yfilt [n]
          #_=> (prn "y" n)
          #_=> (zero? (mod n 5)))
#'resieve.core/yfilt
 (def c (comp (r/filter yfilt) (r/filter xfilt)))
(into [] (c (range 10)))

(defn sieve [pred]
  (fn [f1]
    (let [sieves (atom (fn [] ()))]
      (fn [result input]
        (let [p (@sieves input)]
          (swap! sieves (comp (pred p)) @sieves))
          (f1 result p))))))


(defn -main
    [& args]
    (prn "sieve" (sieve 12))
    (prn "lazy" (take 5 (primes)))
    ;(prn "reducers" (into [] (r/take 5 (r/filter rprime (range))))))
    ; (pprint  (pmap #(`(% (keyword (digest/md5 %)))) (mapcat #(walkf %) args)))
    ; (shutdown-agents)
    ; (    )
)

