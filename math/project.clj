(ns math.project
  (:use math.linalg
        clojure.contrib.math
        zutil.csv))

(defn mag
  "Finds the 'magnitude' of a matrix, which is the entry of the matrix
  that has the largest absolute value. Used to compute errors in
  calculations"
  [A]
  (apply max (map abs (:data A))))

;; PART 1 - QR Decomposition

;; The findings show that the Householder method of QR decomposition
;; is a more stable algorithm than the Gram-Schmidt process. As the
;; size of the matrix increases, the error of the Gram-Schmidt
;; decomposition increases rather dramatically, while the Householder
;; decomposition has a low error that remains almost constant as the
;; size of the matrix increases. The findings show that Householder
;; matrices are well-conditioned, while the Gram-Schmidt process as
;; implemented is ill-conditioned.

(defn part1-input
  "Generates the input matrix for part 1 of the project."
  [n]
  (gen-matrix n n
              #(/ (+ %1 %2 -1))))

(defn q-quality
  "Checks the quality of the Q matrix by seeing how close Q^t*Q is to
  the identity matrix."
  [Q]
  (mag (subt (mult (transpose Q) Q)
             (id (:rows Q)))))

(defn qr-quality
  "Checks the quality of the entire decomposition by seeing how close
  QR is to A."
  [A Q R]
  (mag (subt (mult Q R) A)))

(defn part1 []
  (for [n (range 3 21)]
    (let [A (part1-input n)
          [Qg Rg] (qr-gram-schmidt A)
          [Qh Rh] (qr-householder A)]
      {:n n
       :matrix A
       :gram-schmidt {:Q Qg
                      :R Rg
                      :Q-quality (q-quality Qg)
                      :QR-quality (qr-quality A Qg Rg)}
       :householder {:Q Qh
                     :R Rh
                     :Q-quality (q-quality Qh)
                     :QR-quality (qr-quality A Qh Rh)}})))

(defn part1-output [p1]
  (for [data p1]
    [(data :n)
     ((data :gram-schmidt) :Q-quality)
     ((data :gram-schmidt) :QR-quality)
     ((data :householder) :Q-quality)
     ((data :householder) :QR-quality)]))

;; PART 2 - Solving Ax = b

(defn part2-input [n]
  [(part1-input n)
   (gen-matrix n 1
               (fn [_ __]
                 (expt 0.1 (/ n 3))))])

(defn part2
  ([]
     (for [n (range 3 21)]
       (part2 n)))
  ([n]
     (let [[A b] (part2-input n)]
       {:n n
        :gauss (gauss A b)
        :gauss-partial (gauss-partial A b)
        :qr-gram-schmidt (solve-qr (qr-gram-schmidt A) b)
        :qr-householder (solve-qr (qr-householder A) b)
        :jacobi (jacobi A b)
        :gauss-seidel (gauss-seidel A b)})))

(defn part3-input-row [n i random]
  (let [[r vals]
        (loop [j (dec n)
               random random
               coll []]
          (if (= j 0)
            [random coll]
            (let [r (mod (* 3.0 random) 1.0)]
              (recur (dec j)
                     r
                     (conj coll (- (* 2.0 r) 1.0))))))]
    [(concat (take i vals)
             [(let [sum (reduce + (map abs vals))]
                (if (< r 0.5) sum (- sum)))]
             (drop i vals)) r]))

(defn part3-input-vector [n random]
  (loop [i 0
         random random
         coll []]
    (if (= i n)
      (apply cvec coll)
      (let [r (mod (* 3.0 random) 1.0)]
        (recur (inc i) r (conj coll (- (* 2.0 r) 1.0)))))))

(defn part3-input [n]
  (loop [i 0
         random 0.123456789
         coll []]
    (if (= i n)
      [(matr n n coll) (part3-input-vector n random)]
      (let [[vals r] (part3-input-row n i random)]
        (recur (inc i) r (concat coll vals))))))

(defmacro runtime [expr]
  `(let [start# (System/currentTimeMillis)]
     (let [res# ~expr]
       [res# (- (System/currentTimeMillis) start#)])))

(defn part3
  ([]
     (for [n (concat (range 2 21) [50 100])]
       (part3 n)))
  ([n]
     (let [[A b] (part3-input n)]
       {:n n
        :input [A b]
        :gauss (runtime (gauss A b))
        :gauss-partial (runtime (gauss-partial A b))
        :qr-gram-schmidt (runtime (solve-qr (qr-gram-schmidt A) b))
        :qr-householder (runtime (solve-qr (qr-householder A) b))
        :jacobi (runtime (jacobi A b))
        :gauss-seidel (runtime (gauss-seidel A b))})))

(defn part3-error [A x b]
  (mag (subt (mult A x) b)))

(defn part3-output [p3]
  (for [data p3]
    (let [[A b] (data :input)]
      [(data :n)
       (part3-error A ((data :gauss) 0) b)
       ((data :gauss) 1)
       (part3-error A ((data :gauss-partial) 0) b)
       ((data :gauss-partial) 1)
       ;(part3-error A ((data :qr-gram-schmidt) 0) b)
       ;((data :qr-gram-schmidt) 1)
       (part3-error A ((data :qr-householder) 0) b)
       ((data :qr-householder) 1)
       (part3-error A ((data :jacobi) 0) b)
       ((data :jacobi) 1)
       (part3-error A ((data :gauss-seidel) 0) b)
       ((data :gauss-seidel) 1)])))