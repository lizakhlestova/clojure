(ns task5.core
  (:gen-class))

(def n-cpu (.availableProcessors (Runtime/getRuntime)))

(defn- take-part [coll begin end]
  (take (inc (- end begin)) (drop begin coll)))

(defn- round [x]
  (Math/round (double x)))

(defn split-coll [coll n]
  (let [size (count coll),
        batch-size (max 1 (round (/ size n)))]
    (concat
      (reduce
        (fn [acc i]
          (concat acc (list
                        (take-part coll (* i batch-size)
                          (dec (* (inc i) batch-size))))))
        (list)
        (range (dec n)))
      (if (> size n)
        (list (take-last
                (- size (* (dec n) batch-size))
                coll))
        (list (list))))))

(defn pfilter 
  ([f coll]
   (pfilter f coll (dec n-cpu)))
  ([f coll n]
   (mapcat
     deref
     (doall
       (map
         #(future (doall (filter f %)))
         (split-coll coll n))))))

;---------------------5.2---------------------

(defn- filter-new-batch [prev coll f]
  (let [prev-batch-end (second prev)
        batch-size (last prev)
        batch (take-part coll prev-batch-end
                (dec (+ prev-batch-end batch-size)))]   
    (if (not= batch (list))
      [(pfilter f batch)         
       (+ prev-batch-end batch-size)
       (round (* 1.1 batch-size))]
      [(list) -1 0])))

(defn pfilter-lazy [f coll]
  (let [init-bs (* (dec n-cpu) 10)]
    (if (counted? coll)
        (pfilter f coll)
        (mapcat
          first
            (take-while
              #(not= (second %) -1)
              (cons [(list) 0 0]
                (iterate
                  #(filter-new-batch % coll f)
                  (filter-new-batch [(list) 0 init-bs] coll f))))))))
     

(defn -main []
  (load-file "src/task5/test.clj")
  (shutdown-agents))
