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
    (if (nil? n) n
      (if (zero? (mod n p) n))))

(defn pfilt [p n]
  (zero? (mod n p)))

(defn sieve [pred]
  (fn [f1]
    (let [sieves (atom (fn [] ()))]
      (fn [result input]
        (let [p (r/filter @sieves input)]
          (swap! sieves (comp (r/filter (partial pred p)) @sieves))
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

