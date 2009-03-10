;;; MATRIX ARITHMETIC
;;;
;;; The basic addition, subtraction, and multiplication operations on
;;; matrices and vectors.

(in-ns 'math.linalg)

(with-test
    (defmulti
      #^{:doc "Add two numbers, matrices, etc."}
      plus math-dispatch)
  (is (= (cvec 4 6)
         (plus (cvec 1 2) (cvec 3 4))))
  (is (= (matr 2 2 [1 3 5 7])
         (plus (matr 2 2 (range 4))
               (matr 2 2 (range 1 5)))))
  (is (= (matr 2 2 (range 1 5))
         (plus 1 (matr 2 2 (range 4)))))
  (is (= (matr 2 2 (range 1 5))
         (plus (matr 2 2 (range 4)) 1)))
  (is (= 4 (plus 1 3))))

(defmethod plus [::Matrix ::Matrix] [m1 m2]
  (assert (= (dim m1) (dim m2)))
  (gen-matrix (:rows m1)
              (:cols m1)
              #(+ (mget m1 %1 %2)
                  (mget m2 %1 %2))))

(defmethod plus [::Matrix Number] [m n]
  (gen-matrix (:rows m)
              (:cols m)
              #(+ n (mget m %1 %2))))

(defmethod plus [Number ::Matrix] [n m]
  (plus m n))

(defmethod plus [Number Number] [n1 n2]
  (+ n1 n2))

(defn add
  "Adds a variable number of items using [plus]."
  ([] 0)
  ([x] x)
  ([x & more]
     (reduce plus x more)))

(defmulti
  #^{:doc "Subtracts two numbers, matrices, etc."}
  minus math-dispatch)

(defmethod minus [::Matrix ::Matrix] [m1 m2]
  (assert (= (dim m1) (dim m2)))
  (gen-matrix (:rows m1)
              (:cols m1)
              #(- (mget m1 %1 %2)
                  (mget m2 %1 %2))))

(defmethod minus [::Matrix Number] [m n]
  (gen-matrix (:rows m)
              (:cols m)
              #(- (mget m %1 %2) n)))

(defmethod minus [Number Number] [n1 n2]
  (- n1 n2))

(defn subt
  "Subtracts a variable number of items using [minus]."
  ([] 0)
  ([x] x)
  ([x & more]
     (reduce minus x more)))

(defmulti
  #^{:doc "Multiplies two numbers, matrices, etc."}
  times math-dispatch)

(declare dot) ; This is defined in linalg_ops

(defmethod times [::Matrix ::Matrix] [m1 m2]
  (assert (= (:cols m1) (:rows m2)))
  (gen-matrix (:rows m1)
              (:cols m2)
              (fn [i j]
                (dot (row m1 i)
                     (col m2 j)))))

(defmethod times [::Matrix Number] [m n]
  (gen-matrix (:rows m)
              (:cols m)
              #(* n (mget m %1 %2))))

(defmethod times [Number ::Matrix] [n m]
  (times m n))

(defmethod times [Number Number] [n1 n2]
  (* n1 n2))

(defn mult
  "Multiplies a variable number of items using [times]."
  ([] 1)
  ([x] x)
  ([x & more]
     (reduce times x more)))