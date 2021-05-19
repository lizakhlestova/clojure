(ns task1.task1_3
  (:gen-class))

(defn my-map [f coll]
  (reduce (fn [x y] (concat x (list (f y))))
    (list)
    coll))

(defn my-filter [f coll]
  (reduce (fn [x y] (if (f y) (concat x (list y)) x))
    (list)
    coll))

