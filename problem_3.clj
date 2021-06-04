(defn indef-int [f]
  (let [eps 1e-1]
    (fn [x]
      (->> (range (int (/ x eps)))
        (map #(* eps 0.5 (+ (f (* % eps)) (f (* (inc %) eps)))))
        (reduce + 0.)
        (+ (* (mod x eps) 0.5 (+ (f x) (f (- x (mod x eps))))))))))

(defn indef-int-mem [f]
  (let [eps 1e-1
        F (memoize
            (fn [hack x]
              (if (> x eps)
                (+ (hack hack (- x eps)) (* eps 0.5 (+ (f x) (f (- x eps)))))
                (* x 0.5 (+ (f x) (f 0.))))))]
    (partial F F)))

(defn indef-int-lazy [f]
  (let [eps 1e-1
        part (map last
               (iterate
                  (fn [[n, F_n]] [(inc n), (+ F_n (* eps 0.5 (+ (f (* n eps)) (f (* (inc n) eps)))))])
                  [0 0.0]))]
    (fn [x]
      (+
        (nth part (int (/ x eps)))
        (* (mod x eps) 0.5 (+ (f x) (f (- x (mod x eps)))))))))

(require '[clojure.test :refer :all])
(let [sin #(Math/sin %)
      cos #(Math/cos %)
      exp #(Math/exp %)
      eps 1e-1]
  (deftest test-integral-unoptimized
    (is (every? #(< (- (sin %)       ((indef-int      cos) %)) (* eps eps)) (range 10)))
    (is (every? #(< (- (- (exp %) 1) ((indef-int      exp) %)) (* eps eps)) (range 10))))
  (deftest test-integral-mem
    (is (every? #(< (- (sin %)       ((indef-int-mem  cos) %)) (* eps eps)) (range 10)))
    (is (every? #(< (- (- (exp %) 1) ((indef-int-mem  exp) %)) (* eps eps)) (range 10))))
  (deftest test-integral-lazy
    (is (every? #(< (- (sin %)       ((indef-int-lazy cos) %)) (* eps eps)) (range 10)))
    (is (every? #(< (- (- (exp %) 1) ((indef-int-lazy exp) %)) (* eps eps)) (range 10)))))
(run-tests)
(println)

(defn indef-int-mem-debug [f]
  (let [eps 1e-1
        F (memoize
            (fn [hack x]
              (do
                (printf "%.3f\n" x)
                (if (> x eps)
                  (+ (hack hack (- x eps)) (* eps 0.5 (+ (f x) (f (- x eps)))))
                  (* x 0.5 (+ (f x) (f 0.)))))))]
    (partial F F)))

(def int-cos-debug (indef-int-mem-debug #(Math/cos %)))
(println "(int-cos-debug 1.0)")
(int-cos-debug 1.)
(println)
(println "(int-cos-debug 1.1)")
(int-cos-debug 1.1)
