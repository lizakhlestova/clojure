(ns task4.logic
  (:gen-class)
  (:require [task4.variables :as v]))

(def variable v/variable)
(def variable? v/variable?)
(def var-name v/variable-name)
(def same-vars? v/same-variables?)

(declare logic-expr?)

(defn args [expr]
  {:pre [(logic-expr? expr)]}
  "Get the arguments of the expression.
  
  Parameters
  ----------
  expr: logical expression
  Returns
  -------
  list of logical expressions"
  (rest expr))

(defn- collapse-and-constants [exprs]
  (if (not-any? #(= % false) exprs)
    (let [new-exprs (filter #(not= % true) exprs)]
      (cond
        (empty? new-exprs) true
        (= 1 (count new-exprs)) (first new-exprs)
        :else (cons ::and new-exprs)))
    false))

(defn- collapse-or-constants [exprs]
  (if (not-any? #(= % true) exprs)
    (let [new-exprs (filter #(not= % false) exprs)]
      (cond
        (empty? new-exprs) false
        (= 1 (count new-exprs)) (first new-exprs)
        :else (cons ::or new-exprs)))
    true))

(declare l-or l-not)

(defn- collapse-imply-constants [exprs]
  (if (= true (last exprs))
    true
    (let [premise (collapse-and-constants (drop-last exprs))]
      (l-or (l-not premise) (last exprs)))))

(defn- collapse [exprs replace-func]
  (if (seq? exprs)
    (reduce
      (fn [acc expr]
        (concat acc (replace-func expr)))
      (replace-func (first exprs))
      (rest exprs))
    exprs))

(declare or? and?)

(defn- replace-or-w-list [expr]
  (if (or? expr)
    (args expr)
    (list expr)))

(defn- replace-and-w-list [expr]
  (if (and? expr)
    (args expr)
    (list expr)))

(defn- check-args [args]
  (not (some #(not (logic-expr? %)) args)))
  

(defn l-and [expr & rest]
  "Construct a new conjunction logical expression.
  
  Parameters
  ----------
  expr & rest: logical expressions
  Returns
  -------
  logical expression"
  {:pre [(check-args (cons expr rest))]}
  (if (empty? rest)
    expr
    (collapse-and-constants
      (collapse (cons expr rest) replace-and-w-list))))

(defn and? [expr]
  "Check if the expression is a conjunction.
  
  Parameters
  ----------
  expr: any type
  Returns
  -------
  boolean"
  (if (seq? expr)
    (= ::and (first expr))
    false))

(defn l-or [expr & rest]
  "Construct a new disjunction logical expression.
  
  Parameters
  ----------
  expr & rest: logical expressions
  Returns
  -------
  logical expression"
  {:pre [(check-args (cons expr rest))]}
  (if (empty? rest)
    expr
    (collapse-or-constants
      (collapse (cons expr rest) replace-or-w-list))))

(defn or? [expr]
  "Check if the expression is a disjunction.
  
  Parameters
  ----------
  expr: any type
  Returns
  -------
  boolean"
  (if (seq? expr)
    (= ::or (first expr))
    false))

(defn l-not [expr]
  "Construct a new negation logical expression.
  
  Parameters
  ----------
  expr: logical expressions
  Returns
  -------
  logical expression"
  {:pre [(logic-expr? expr)]}
  (cond
    (= true expr) false
    (= false expr) true
    :else (cons ::not (list expr))))

(defn not? [expr]
  "Check if the expression is a negation.
  
  Parameters
  ----------
  expr: any type
  Returns
  -------
  boolean"
  (if (seq? expr)
    (= ::not (first expr))
    false))

(defn imply [expr & rest]
  "Construct a new implication logical expression.
  
  Parameters
  ----------
  expr & rest: logical expressions
  Returns
  -------
  logical expression"
  {:pre [(check-args (cons expr rest))]}
  (if (empty? rest)
    (l-not expr)
    (collapse-imply-constants (cons expr rest))))

(defn imply? [expr]
  "Check if the expression is an implication.
  
  Parameters
  ----------
  expr: any type
  Returns
  -------
  boolean"
  (or? expr))


(defn logic-expr? [expr]
  "Check if the expression is a logical expression.
  
  Parameters
  ----------
  expr: any type
  Returns
  -------
  boolean"
  (or
      (boolean? expr)
      (variable? expr)
      (not? expr)
      (and? expr)
      (or? expr)
      (imply? expr)))

(defn- operation [expr]
  (cond 
    (and? expr) #(apply l-and %)
    (or? expr) #(apply l-or %)
    (not? expr) #(l-not (first %))
    :else identity))

(defn substitute [expr substitutions]
  "Substitute the occurrences of certain variables
  in the expression.
  
  Parameters
  ----------
  expr: logical expression
  substitutions: list of pairs of a variable and a logical expression
      The first element of a pair is the variable to be substituted,
      and the second element is a logical expression which is supposed
      to be a substitute.
  Returns
  -------
  logical expression
      Result of the substitution."
  {:pre [(logic-expr? expr)]}
  (if (variable? expr)
    (second (some (fn [subst]
                    (if (same-vars? (first subst) expr)
                      subst
                      false))
              (concat substitutions (list [expr expr]))))
    (if (boolean? expr)
      expr
      ((operation expr)
       (map
         #(substitute % substitutions)
         (args expr))))))
