(ns task4.fdnf
  (:gen-class)
  (:require [task4.variables :as v])
  (:require [task4.logic :as l]))

(defn- get-variables
  ([expr] 
   (get-variables expr (list)))
  ([expr res-coll]
   (if (v/variable? expr)
     (concat res-coll (list expr))
     (if (boolean? expr)
       res-coll
       (distinct
         (reduce
           (fn [x y] (get-variables y x))
           res-coll
           (l/args expr)))))))

(defn- decart-product [coll pair]
  (map
    (fn [x] [(concat (first x) (list (first pair)))
             (concat (second x) (list (second pair)))])
    coll))

(defn- get-assignations-pre [expr]
  (let [coll (get-variables expr)]
    (reduce
      (fn [x y] (concat
                  (decart-product x [y true])
                  (decart-product x [(l/l-not y) false])))
      (list [(list (first coll)) (list true)]
        [(list (l/l-not (first coll))) (list false)])
      (rest coll))))

(defn- get-assignations [expr]
  (let [vars (get-variables expr)]
    (map
      (fn [pair]
        [(apply l/l-and (first pair))
         (map vector vars (second pair))])
      (get-assignations-pre expr))))

(defn fdnf [expr]
  "Convert the expression to the full disjunctive normal form (FDNF).
  
  Parameters
  ----------
  expr: logical expression
  Returns
  -------
  logical expression
      A FDNF that is an equivalent to the expression."
  {:pre [(l/logic-expr? expr)]}
  (apply l/l-or (map
                  first
                  (filter
                    #(= true (l/substitute expr (second %)))
                    (get-assignations expr)))))
