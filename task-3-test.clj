(ns task3.test
  (:gen-class)
  (:require [clojure.test :as test])
  (:require [task3.core :as core]))

(defn id [x]
  x)

(defn sin [x]
  (Math/sin x))

(defn eq [x y]
  (< (Math/abs (- x y)) 1e-4))

(test/deftest basic-test
  (test/testing "Basic functions"
    (test/is (= (core/calc-trapezoid id 0 0) 0))
    (test/is (= (core/calc-trapezoid id 0 1) 1/2))
    (test/is (eq (core/calc-trapezoid sin 0 Math/PI) 0))
    (test/is (= (core/integrate id 0 0) 0))
    (test/is (eq (core/integrate id 0 1) 1/2))
    (test/is (eq (core/integrate id -1 1) 0))
    (test/is (eq (core/integrate sin -1 1) 0))
    (test/is (eq (core/integrate sin 0 (/ Math/PI 2)) 1))))

(test/deftest mem-test
  (test/testing "Memoize"
    (test/is (= (core/m-integrate id 0 0) 0))
    (test/is (eq (core/m-integrate id 0 1) 1/2))
    (test/is (eq (core/m-integrate sin -1 1) 0))
    (test/is (eq (core/m-integrate sin 0 (/ Math/PI 2)) 1))))

(test/deftest stream-test
  (test/testing "Stream"
    ;(println "\n\n" (take 20 (core/stream-integrate sin 1e-2)))
    (test/is (eq (second (last (core/stream-integrate sin 1e-5 (/ Math/PI 2)))) 1))))
    

(defn my-print [name val]
  (println (format "%s: %.8f" name (double val))))

(test/deftest time-test
  (test/testing "Time"
    (println "wo memoize")
    (time (doall (map #(core/integrate sin 0 %) (range 0 3 0.01))))
    (println "w memoize 1")
    (time (doall (map #(core/m-integrate sin 0 %) (range 0 3 0.01))))
    (println "w memoize 2")
    (time (doall (map #(core/m-integrate sin 0 %) (range 0 3 0.01))))))
    ;(time (my-print "basic" (core/integrate sin 0 (* 10 (/ Math/PI 2)))))
    ;(time (my-print "basic" (core/integrate sin 0 (* 10 (/ Math/PI 2)))))
    ;(println "----------------------------")
    ;(time (my-print "memoized" (core/m-integrate sin 0 (* 10 (/ Math/PI 2)))))
    ;(time (my-print "memoized" (core/m-integrate sin 0 (* 10 (/ Math/PI 2)))))
    ;(time (my-print "memoized" (core/m-integrate sin 0 (* 10 (/ Math/PI 2)))))
    ;(time (my-print "memoized" (core/m-integrate sin 0 (+ (* 10 (/ Math/PI 2)) 0.01235))))
    ;(println "----------------------------")
    ;(let [integrate-sin (core/s-integrate sin 1e-2)]
      ;(time (my-print "stream"(integrate-sin (/ Math/PI 2))))
      ;(time (my-print "stream"(integrate-sin (/ Math/PI 2))))
      ;(time (my-print "stream"(integrate-sin (/ Math/PI 2))))
      ;(time (my-print "stream"(integrate-sin (+ (/ Math/PI 2) 0.01235)))))))  

(test/run-tests 'task3.test)
