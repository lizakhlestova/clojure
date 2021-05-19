(ns task1.task1_4
  (:gen-class))

(defn decart_product [str1 alphabet]
  (map (fn [x] (.concat str1 x))
       (filter (fn [x] (not (.endsWith str1 x))) alphabet)))

(defn decart_product_total [coll alphabet]
  (reduce (fn [x y] (concat x y))
    (list)
    (map (fn [x] (decart_product x alphabet))
      coll)))
    

(defn get_substrings [alphabet n]
  (reduce (fn [x y] (decart_product_total x y))
    (list "")
    (repeat n alphabet)))

