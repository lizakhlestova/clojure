(ns task4.dnf
  (:gen-class)
  (:require [task4.variables :as v])
  (:use [task4 logic]))

(declare dnf)

(defn- not-rule [expr]
  (let [arg (second expr)]
    (cond 
      (and? arg) (dnf (apply l-or (map l-not (args arg))))
      (or? arg) (dnf (apply l-and (map l-not (args arg))))
      (not? arg) (dnf (second arg))
      :else expr)))

(defn- wrap-into-list [expr]
  (if (or (v/variable? expr) (boolean? expr) (not? expr))
    (list expr)
    expr))

(defn- and-wo-repetitions [l-expr r-expr]
  (let [wrapped-l-expr (wrap-into-list l-expr)]
    (if (some #(= % r-expr) wrapped-l-expr)
      l-expr
      (if (some #(= % (dnf (l-not r-expr))) wrapped-l-expr)
        false
        (l-and l-expr r-expr)))))

(defn- and-rule-subroutine [arg1 arg2]
  (let [d-arg1 (dnf arg1)
        d-arg2 (dnf arg2)]
    (cond
      (or? d-arg1) (dnf (apply l-or
                          (map
                            #(l-and % d-arg2)
                            (args d-arg1))))
      (or? d-arg2) (dnf (apply l-or
                          (map
                            #(l-and d-arg1 %)
                            (args d-arg2))))
      :else (and-wo-repetitions d-arg1 d-arg2))))

(defn- and-rule [expr]
  (reduce and-rule-subroutine
    (first (args expr))
    (rest (args expr))))

(defn- or-rule [expr]
  (apply l-or (map
                #(dnf %)
                (distinct (args expr)))))    

(def dnf-rules
  (list
    [#(boolean? %)
     identity]
    [#(v/variable? %)
     identity]
    [#(not? %)
     not-rule]
    [#(and? %)
     and-rule]
    [#(or? %)
     or-rule]))
    

(defn dnf [expr]
  "Convert the expression to a disjunctive normal form (DNF).
  
  Parameters
  ----------
  expr: logical expression
  Returns
  -------
  logical expression
      A DNF that is an equivalent to the expression."
  {:pre [(logic-expr? expr)]}
  ((some (fn [rule]
           (if ((first rule) expr)
             (second rule)
             false))
     dnf-rules)
   expr))
