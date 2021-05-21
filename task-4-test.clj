(ns task4.test
  (:gen-class)
  (:require [clojure.test :as test])
  (:require [task4.logic :as l])
  (:require [task4.variables :as v])
  (:require [task4.fdnf :as f])
  (:require [task4.dnf :as d]))

(def variable v/variable)
(def variable? v/variable?)
(def var-name v/variable-name)
(def same-vars? v/same-variables?)
(def substitute l/substitute)

(test/deftest type-test
  (test/testing "lecture assertions"
    (test/is (variable? (variable :x)))
    (test/is (= :x (var-name (variable :x))))
    (test/is (same-vars?
               (variable :x)
               (variable :x)))
    (test/is (not (same-vars?
                    (variable :x)
                    (variable :y))))
    (test/is (not (variable? true)))
    (test/is (not (variable? (list 1))))
    (test/is (not= true (v/variable :x)))
    ;---------------------------------
    (test/is (boolean? (boolean 1)))
    (test/is (= false (boolean false)))))


(test/deftest logic-expr-test
  (test/testing "logical expressions"
    (test/is (l/and? (l/l-and (variable :x) (variable :y) (variable :x))))
    (test/is (l/or? (l/l-or (variable :x) (variable :y) (variable :x))))
    (test/is (l/imply? (l/imply (variable :x) (variable :y) (variable :x))))
    (test/is (l/not? (l/l-not (variable :x))))
    (test/is (= (variable :x) (l/l-and (variable :x))))
    (test/is (= false (l/l-and true false true)))
    (test/is (= true (l/l-or false false true)))
    (test/is (= true (l/l-not false)))
    (test/is (= true (l/imply false true)))
    (test/is (= false (l/imply true true false)))))


;(1 && x && y) || (1 && y && not(z)) || 0 || (x -> z)
(def expr (l/l-or
                (l/l-and true (variable :x) (variable :y))
                (l/l-and true (variable :y) (l/l-not (variable :z)))
                false
                (l/imply (variable :x) (variable :z))))

;(x || y || z) && not(x && y && z)
(def expr2 (l/l-and
             (l/l-or (variable :x) (variable :y) (variable :z))
             (l/l-not (l/l-and (variable :x) (variable :y) (variable :z)))))

;(x || (x -> not(y) -> z)) && not(not(x) || y || not(z))
(def expr3 (l/l-and
             (l/l-or
               (variable :x)
               (l/imply (variable :x) (l/l-not (variable :y)) (variable :z)))
             (l/l-not
               (l/l-or
                 (l/l-not (variable :x))
                 (variable :y)
                 (l/l-not (variable :z))))))

(def expr4 (l/imply
             (l/l-not expr3)
             (l/l-and expr expr2)))

(test/deftest substitute-test
  (test/testing "substitutions"
    (test/is (= false (substitute
                        (variable :x)
                        (list [(variable :x) false]))))
    (test/is (= (variable :y) (substitute
                                (variable :x)
                                (list [(variable :x) (variable :y)]))))
    (test/is (= (variable :x) (substitute
                                (variable :x)
                                (list [(variable :y) false]))))
    (test/is (= true (substitute 
                       expr
                       (list
                         [(variable :x) false]
                         [(variable :y) true]
                         [(variable :z) true]))))))

(test/deftest dnf-test
  (test/testing "dnf"
    (println (d/dnf expr2))
    ;(println (f/fdnf expr))
    (test/is (= (f/fdnf expr) (f/fdnf (d/dnf expr))))
    (test/is (= (f/fdnf expr2) (f/fdnf (d/dnf expr2))))
    (test/is (= (f/fdnf expr3) (f/fdnf (d/dnf expr3))))
    ;(println (d/dnf expr4))
    (test/is (= (f/fdnf expr4) (f/fdnf (d/dnf expr4))))))

(test/run-tests 'task4.test)
