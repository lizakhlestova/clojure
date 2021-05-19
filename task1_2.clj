(ns task1.task1_2
  (:gen-class))

(defn decart_product 
  ([str1 alphabet] 
   (decart_product str1 alphabet (list)))
  ([str1 alphabet res-coll] 
   (if (> (count alphabet) 0)
     (if (not (.endsWith str1 (first alphabet)))
       (recur str1 (rest alphabet)
         (concat res-coll (list (.concat str1 (first alphabet)))))
       (recur str1 (rest alphabet) res-coll))
     res-coll)))

(defn decart_product_total 
  ([coll alphabet]
   (decart_product_total coll alphabet (list)))
  ([coll alphabet res-coll]
   (if (> (count coll) 0)
     (recur (rest coll) alphabet 
       (concat res-coll (decart_product (first coll) alphabet)))
     res-coll))) 

(defn get_substrings [alphabet n init]
  (if (> n 0)
    (recur alphabet (dec n) (decart_product_total init alphabet))
    init))

