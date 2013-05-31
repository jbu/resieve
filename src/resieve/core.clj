(ns resieve.core
  (:require [clojure.core.reducers :as r])
  (gen-class))


(defn ^:dynamic sieve 
    ([n] (sieve [] (range 2 n)))
    ([primes xs]
       (if-let [prime (first xs)]
             (recur (conj primes prime) (remove #(zero? (mod % prime)) xs))
             primes)))

(defn ^:dynamic primes
    ([] (primes (iterate inc 2)))
    ([s] (cons (first s)
         (lazy-seq (primes (remove #(zero? (mod % (first s))) (rest s)))))))

(defn sievereduce [coll]
  (fn [f1]
    (fn [result input]
      (let [p (first input)]
        (f1 result input)
        p))))

(defn -main
    [& args]
    (prn "sieve" (sieve 12))
    (prn "lazy" (take 5 (primes)))
    (prn "reducers" (into [] (r/take 5 (sievereduce (range))))))
    ; (pprint  (pmap #(`(% (keyword (digest/md5 %)))) (mapcat #(walkf %) args)))
    ; (shutdown-agents)
    ; (    )
