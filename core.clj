(ns task1.core
  (:gen-class)
  (:require [task1.task1_2 :as r])
  (:require [task1.task1_3 :as m])
  (:require [task1.task1_4 :as mr]))

(defn decart_product [str1 alphabet]
  (if (> (count alphabet) 0)
    (if (not (.endsWith str1 (first alphabet)))
      (cons (.concat str1 (first alphabet))
        (decart_product str1 (rest alphabet)))
      (decart_product str1 (rest alphabet)))
    (list)))

(defn decart_product_total [coll alphabet]
  (if (> (count coll) 0) 
    (concat (decart_product (first coll) alphabet) 
      (decart_product_total (rest coll) alphabet))
    (list))) 

(defn get_substrings [alphabet n init]
  (if (> n 0)
    (get_substrings alphabet (dec n) (decart_product_total init alphabet))
    init))

(defn -main [] 

  ;(println (decart_product "string" (list "a" "b" "c" "g" "k")))
  ;(println (decart_product_total (decart_product "string" (list "a" "b" "c" "g" "k"))
  ;           (list "1" "2" "3" "4")]
  

  (println (m/my-map #(+ % 10) (range 10)))
  (println (m/my-filter #(zero? (mod % 3)) (range 30)))
  
  (println (get_substrings (list "a" "b" "c" "d") 3 (list "")))
  (println (r/get_substrings (list "a" "b" "c" "d") 3 (list "")))
  (println (mr/get_substrings (list "a" "b" "c" "d") 3)))
