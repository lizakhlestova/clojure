(ns task5.test
  (:gen-class)
  (:require [clojure.test :as test])
  (:require [task5.core :as core])
  (:use [task5 primes]))

(def pfilter core/pfilter)
(def pfilter-lazy core/pfilter-lazy)

(test/deftest basic-test
  (test/testing "basic"
    (test/is (= (core/split-coll (range 7) 3)
                (list (list 0 1) (list 2 3) (list 4 5 6))))
    (test/is (= (core/split-coll (range 4) 7)
                (list (list 0) (list 1) (list 2) (list 3) (list) (list) (list))))
    (test/is (= (core/split-coll (range 50) 1)
                (list (range 50))))
    (test/is (= (pfilter even? (range 1e4)) (filter even? (range 1e4))))
    (test/is (= (pfilter-lazy even? (range 1e3)) (filter even? (range 1e3))))
    (test/is (= (take 500 (pfilter-lazy even? (iterate inc 0)))
                (filter even? (range 1000))))))
    ;(test/is (= (nth primes 10000) (nth p-primes 10000)))))


(test/deftest time-test
  (test/testing "profiling"
    (time (dotimes [_ 20]
            (doall (filter
                     #(> (apply + %) 1e6)
                     (repeat 100 (range 10000))))))
    (dotimes [n 20] (do
                      (println (inc n))
                      (time (dotimes [_ 20]
                              (pfilter #(> (apply + %) 1e6) 
                                (repeat 100 (range 10000)) (inc n))))))
    (println "\n--------default filter--------")
    (time (doall (take 10 primes)))
    (time (doall (take 1000 primes)))
    (time (doall (take 10000 primes)))
    (time (doall (take 20000 primes)))
    (time (doall (take 30000 primes)))
    (time (doall (take 100000 primes)))
    (println "\n-------parallel filter--------")
    (time (doall (take 10 p-primes)))
    (time (doall (take 1000 p-primes)))
    (time (doall (take 10000 p-primes)))
    (time (doall (take 20000 p-primes)))
    (time (doall (take 30000 p-primes)))
    (time (doall (take 100000 p-primes)))))
  

(test/run-tests 'task5.test)
