(ns task5.primes
  (:gen-class)
  (:require [task5.core :as core]))

(declare primes)
(def primes
  (filter (fn [x] (not-any? #(zero? (mod x %))
                    (take-while #(<= % (Math/sqrt x)) 
                      (cons 2 (iterate #(+ 2 %) 3)))))
    (iterate inc 2)))


(def p-primes
  (core/pfilter-lazy
    (fn [x] (not-any? #(zero? (mod x %))
              (take-while #(<= % (Math/sqrt x)) 
                (cons 2 (iterate #(+ 2 %) 3)))))
    (iterate inc 2)))
