(ns task2.test
  (:gen-class)
  (:require [clojure.test :as test])
  (:require [task2.core :as core]))

(test/deftest def-test
  (test/testing "Test"
    (test/is (= (core/sqr 0) 0))
    (test/is (= (core/sqr 1) 1))
    (test/is (= (core/sqr 9) 81))
    (test/is (= (core/cross-out (range 2 100))
                (cons 2 (filter odd? (range 2 100)))))
    (test/is (= (core/sieve (range 2 30))
                (list 2 3 5 7 11 13 17 19 23 29)))
    (test/is (= (core/get-nth-prime -10) nil))
    (test/is (= (core/get-nth-prime 0) nil))
    (test/is (= (core/get-nth-prime 1) 2))
    (println (map core/get-nth-prime (range 1 101)))
    (test/is (= (core/get-nth-prime 5) 11))
    (test/is (= (core/get-nth-prime 30000) 350377))
    ;(test/is (= (nth core/primes 29999) 350377))
    (test/is (= (nth core/primes 99) 541))))

(test/run-tests 'task2.test)
