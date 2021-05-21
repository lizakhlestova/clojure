(ns task2.core
  (:gen-class)
  (:require [clojure.core.reducers :as r]))

(defn sqr [x]
  (* x x))

(defn cross-out [coll]
  (filter (fn [x] (or (< x (sqr (first coll)))
                      (not= (mod x (first coll)) 0)))
    coll))

(defn sieve [coll]
  (let [max-it 1000]
    (reduce (fn [x y] (if (<= (sqr (nth x y)) (last x))
                        (concat (take y x) (cross-out (drop y x)))
                        (reduced x)))
      coll
      (range max-it))))
    
(defn get-nth-prime [n]
  (if (> n 0)
    (let [max-size (* (* 1.2 n) (+ (Math/log n) 2)),
          prime-numbers (sieve (range 2 max-size))]
     (nth prime-numbers (dec n)))
    nil))

(declare primes)

(def primes
  (filter (fn [x] (not-any? #(zero? (mod x %))
                    (filter #(<= % (Math/sqrt x)) 
                      primes)))
    (iterate inc 2)))

(defn -main []
  (load-file "src/task2/test.clj") 
  (println "\n" (take 100 primes))
  (time (println "\n" (nth primes 10000)))
  (time (println "\n" (nth primes 10001))))

  
