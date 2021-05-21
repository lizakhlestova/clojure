(ns task4.variables
  (:gen-class))

(defn variable [name]
  "Construct a new variable.
  
  Parameters
  ----------
  name: keyword
  Returns
  -------
  variable"
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  "Check if the expression is a variable.
  
  Parameters
  ----------
  expr: any type
  Returns
  -------
  boolean"
  (if (seq? expr)
    (= (first expr) ::var)
    false))

(defn variable-name [v]
  "Get the name of the variable.
  
  Parameters
  ----------
  v: variable
  Returns
  -------
  keyword"
  (second v))

(defn same-variables? [v1 v2]
  "Check if the variables are the same.
  
  Parameters
  ----------
  v1: variable
  v2: variable
  Returns
  -------
  boolean"
  (and
       (variable? v1)
       (variable? v2)
       (= (variable-name v1)
          (variable-name v2))))
