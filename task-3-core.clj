(ns task3.core
  (:gen-class))

(defn calc-trapezoid [f left step] 
  (* step (/ (+ (f left) (f (+ left step)))  2)))

;(right < left) is not handled
(defn integrate ([f right]
                 (integrate f 0 right))
  ([f left right]
   (let [step 1e-5]
     (reduce + 0
       (map #(calc-trapezoid f % step) (range left right step))))))

(def integrate-mem (memoize integrate))

(defn m-integrate ([f right]
                   (m-integrate f 0 right))
  ([f left right]
   (let [step 0.1]
     (+ (reduce + 0
          (map #(integrate-mem f % (+ % step))
            (range left (- right step) step)))
        (integrate f (- right (mod (- right left) step)) right)))))

;---------3.2---------
(defn limit-stream 
  ([s right]
   (take-while #(<= (+ (first %) 1e-8) right) s))
  ([f s right]
   (let [init-s (limit-stream s right),
         x (first (last init-s)),
         y (second (last init-s))]
     (cons init-s
       (list [right (+ y (integrate f x right))])))))

(defn stream-integrate
  ([f step]
   (iterate (fn [[x y]]
              [(+ x step) (+ y (integrate f x (+ x step)))])
     [0 0]))
  ([f step right]
   (limit-stream f (stream-integrate f step) right)))

(defn s-integrate [f step]
  (let [stream (stream-integrate f step)]
    #(second (last (limit-stream f stream %)))))


(defn -main []
  (load-file "src/task3/test.clj"))
