(ns math.linalg
  (:use clojure.contrib.test-is))

;;; BASIC MATH FUNCTIONS
;;; Not sure why clojure itself doesn't have these

(defn abs [n] (if (< n 0) (- n) n))
(defn avg [& nums]
  (/ (reduce + nums)
     (count nums)))

(defn ** [base exp]
  (if (integer? exp)
    (reduce * (replicate exp base))
    (Math/pow base exp)))

(defn- close-enough? [n1 n2]
  (< (abs (- n1 n2)) 0.000000001))

;;; FINDING SQUARE ROOTS
;;; Java sucks at it, floating point numbers are epic fail.

(defn- is-valid? [guess n]
  (< (abs (- (** guess 2) n))
     0.000000001))

(defn- improve [guess n]
  (avg guess (/ n guess)))

(defn sqrt [num]
  (loop [guess 1
         n num]
    (if (is-valid? guess n)
      guess
      (recur (improve guess n) n))))

;;; Matrix has: width, height, data
;;; for simplicity store data as a single vector

(def print-matrix)
(defstruct matrix :rows :cols :data)

(with-test
    (defn matr
      "Create a matrix with dimensions and data.
  Any other matrix constructors should use this to make sure metadata
  is set correctly."
      [rows cols data]
      (with-meta (assoc (struct matrix rows cols (vec data)) :type ::Matrix)
                 {:custom-print print-matrix}))
  (let [m (matr 3 2 [1 2 3 4 5 6])]
    (is (= 3 (:rows m)))
    (is (= 2 (:cols m)))
    (is (= [1 2 3 4 5 6] (:data m)))))

(with-test
    (defn dim
      "Get the dimensions of the matrix.
  If it's a vector, return a scalar with the length of the vector
  Otherwise, return a Clojure vector with the number of rows and the
  number of columns."
      [m]
      (cond (= (:rows m) 1) (:cols m)
            (= (:cols m) 1) (:rows m)
            true [(:rows m) (:cols m)]))
  (is (= 2 (dim (matr 1 2 [3 4]))))
  (is (= 3 (dim (matr 3 1 [1 2 3]))))
  (is (= [3 4] (dim (matr 3 4 (range 12))))))

(with-test
    (defn cvec
      "Creates a column vector with the data.
  The vector will have 1 column and rows to fit the data."
      [& data] (matr (count data) 1 (vec data)))
  (let [v (cvec 1 2 3)]
    (is (= 3 (dim v)))
    (is (= 3 (:rows v)))
    (is (= 1 (:cols v)))
    (is (= [1 2 3] (:data v)))))

(with-test
    (defn rvec
      "Creates a row vector with the data.
  The vector will have 1 row and columns to fit the data."
      [& data] (matr 1 (count data) (vec data)))
  (let [v (rvec 1 2 3)]
    (is (= 3 (dim v)))
    (is (= 1 (:rows v)))
    (is (= 3 (:cols v)))
    (is (= [1 2 3] (:data v)))))

(with-test
    (defn vec?
      "Is this matrix actually a vector? Does it have only one row or
  only one column?"
      [m] (not (vector? (dim m))))
  (is (vec? (matr 1 1 [3])))
  (is (vec? (matr 1 3 (range 3))))
  (is (vec? (matr 3 1 (range 3))))
  (is (not (vec? (matr 2 3 (range 6))))))

(with-test
    (defn cvec?
      "Is this a column vector?"
      [v] (= 1 (:cols v)))
  (is (cvec? (cvec 1 2 3)))
  (is (cvec? (matr 2 1 [1 2])))
  (is (not (cvec? (rvec 1 2 3)))))

(with-test
    (defn rvec?
      "Is this a row vector?"
      [v] (= 1 (:rows v)))
  (is (rvec? (rvec 1 2 3)))
  (is (rvec? (matr 1 3 [1 2 3])))
  (is (not (rvec? (cvec 1 2 3)))))

(with-test
    (defn- flattened-index [m i j]
      (+ (dec j)
         (* (dec i) (:cols m))))
  (let [m (matr 3 4 (vec (range 12)))]
    (is (= 0 (flattened-index m 1 1)))
    (is (= 1 (flattened-index m 1 2)))
    (is (= 2 (flattened-index m 1 3)))
    (is (= 5 (flattened-index m 2 2)))
    (is (= 8 (flattened-index m 3 1)))
    (is (= 11 (flattened-index m 3 4)))))

(with-test
    (defn mget
      "Get an element of a matrix or vector
  Matrix indexing starts at 1 and ends at the number of rows/columns
  (inclusive)."
      ([m i]
         (assert (vec? m))
         ((:data m) (dec i)))
      ([m i j] ((:data m) (flattened-index m i j))))
  (let [m (matr 2 3 (range 1 7))]
    (is (= 1 (mget m 1 1)))
    (is (= 2 (mget m 1 2)))
    (is (= 3 (mget m 1 3)))
    (is (= 4 (mget m 2 1)))
    (is (= 5 (mget m 2 2)))
    (is (= 6 (mget m 2 3)))
    (is (= 1 (mget (cvec 1 2 3) 1)))
    (is (= 3 (mget (rvec 1 2 3) 3)))))

(with-test
    (defn- expanded-index [m i]
      [(inc (int (Math/floor (/ i (:cols m)))))
       (inc (rem i (:cols m)))])
  (let [m (matr 2 3 (range 6))]
    (is (= [1 1] (expanded-index m 0)))
    (is (= [1 2] (expanded-index m 1)))
    (is (= [1 3] (expanded-index m 2)))
    (is (= [2 2] (expanded-index m 4)))
    (is (= [2 3] (expanded-index m 5)))))

(with-test
    (defn gen-matrix
      "Create a matrix of given dimensions whose data is generated by a
  function. The function should take two arguments, a row index and a
  column index, and return the value that belongs at that position."
      [r c fun]
      (let [m (matr r c [])]
        (assoc m :data (vec (map #(apply fun (expanded-index m %))
                                 (range 0 (* r c)))))))
  (let [m (gen-matrix 2 3 +)]
    (is (= 2 (mget m 1 1)))
    (is (= 3 (mget m 1 2)))
    (is (= 5 (mget m 2 3)))))

(with-test
    (defn mat?
      "Is this a matrix and not a vector?"
      [m] (vector? (dim m)))
  (is (not (mat? (matr 1 3 (range 3)))))
  (is (not (mat? (matr 3 1 (range 3)))))
  (is (not (mat? (matr 1 1 [3]))))
  (is (mat? (matr 2 3 (range 6)))))

(with-test
    (defn square?
      "Does this matrix have matching numbers of rows and columns? This
  is a requirement for a few different operations."
      [m] (and (mat? m)
               (= (:rows m) (:cols m))))
  (is (square? (matr 3 3 (range 9))))
  (is (square? (matr 2 2 (range 4))))
  (is (not (square? (matr 1 1 [1]))))
  (is (not (square? (matr 2 1 (range 2)))))
  (is (not (square? (matr 3 2 (range 6))))))

(with-test
    (defn transpose
      "Transpose the rows and columns of the matrix."
      [m]
      (gen-matrix (:cols m)
                  (:rows m)
                  #(mget m %2 %1)))
  (let [m (matr 2 3 (range 6))
        t (transpose m)]
    (is (= [3 2] (dim t)))
    (is (= [0 3 1 4 2 5] (:data t)))))

(with-test
    (defn dot
      "Take the dot product of two vectors"
      [v1 v2]
      (assert (and (vec? v1) (vec? v2)))
      (assert (= (dim v1) (dim v2)))
      (apply + (for [i (range (dim v1))]
                 (* (mget v1 (inc i))
                    (mget v2 (inc i))))))
  (is (thrown? Exception
               (dot (cvec 1 2)
                    (cvec 1 2 3))))
  (is (thrown? Exception
               (dot (cvec 1 2)
                    (matr 3 2 (range 6)))))
  (is (= 14 (dot (cvec 1 2 3)
                 (rvec 1 2 3))))
  (is (= 10 (dot (cvec 3 2 1)
                 (cvec 1 2 3)))))

(with-test
    (defn row
      "Gets the vector for a row of a matrix"
      [m i] (gen-matrix 1 (:cols m)
                        (fn [_ j] (mget m i j)))))

(with-test
    (defn col
      "Gets the vector for a column of a matrix"
      [m j] (gen-matrix (:rows m) 1
                        (fn [i _] (mget m i j)))))

(defn rows
  "Returns a seq of the row vectors of the matrix"
  [m] (map (partial apply rvec)
           (partition (:cols m) (:data m))))

(defn cols
  "Returns a seq of the column vectors of the matrix"
  [m] (map (partial apply cvec)
           (partition (:rows m)
                      (apply interleave
                             (partition (:cols m) (:data m))))))

;;; MATRIX ARITHMETIC

(defn- dispatcher [a b]
  [(or (:type a)
       (if (= clojure.lang.Ratio (class a))
         Number
         (class a)))
   (or (:type b)
       (if (= clojure.lang.Ratio (class b))
         Number
         (class b)))])

(with-test
    (defmulti
      #^{:doc "Add two numbers, matrices, etc."}
      plus dispatcher)
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
  minus dispatcher)

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
  times dispatcher)

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

;;; DETERMINANT OF A MATRIX

(defmulti
  #^{:doc "Take the determinant of a matrix."}
  det (fn [m]
        (assert (square? m))
        (first (dim m))))

(defmethod det 2 [m]
  (- (* (mget m 1 1)
        (mget m 2 2))
     (* (mget m 1 2)
        (mget m 2 1))))

(defmethod det :default [m]
  (let [opp {+ - - +}
        mm (gen-matrix (:rows m)
                       (* 2 (:cols m))
                       #(mget m % (inc (rem (dec %2) (:cols m)))))]
    (reduce #(+ % (* (mget mm 1 %2)
                     (det (gen-matrix (dec (:rows m))
                                      (dec (:cols m))
                                      (fn [i j]
                                        (mget mm (inc i) (+ %2 j)))))))
            0 (range 1 (inc (:cols m))))))

(defn id
  "Gives an identity matrix with n rows and n columns."
  [n] (gen-matrix n n #(if (= %1 %2) 1 0)))

(defn zero
  "Gives a matrix with m rows and n columns filled with zeroes."
  [m n]
  (gen-matrix m n (constantly 0)))

;;; LINEAR TRANSFORMATIONS

(defn print-xform
  ([o] (print-xform o *out*) (prn))
  ([o w] (.write w (str (pr-str (first (o)))
                        " -> "
                        (pr-str (second (o)))))))

(defmacro xform
  "Create a linear transformation function."
  [args & body]
  `(fn
     ([] (cons '~args '~body))
     ([vec#]
        (let [~args (:data vec#)]
          ~@body))))

(with-test
    (defn std-basis
      "Gives the standard basis vector of the given size and index.
  If no index is given, return a vector of all of them."
      ([size]
         (map (partial std-basis size) (range 1 (inc size))))
      ([size idx]
         (col (id size) idx)))
  (is (= 3 (:rows (std-basis 3 1))))
  (is (= [1 0 0] (:data (std-basis 3 1))))
  (is (= [0 1] (:data (std-basis 2 2))))
  (is (= [0 0 1 0] (:data (std-basis 4 3)))))

(defn- join-cols [col-seq]
  (matr (dim (first col-seq))
        (count col-seq)
        (apply interleave (map :data col-seq))))

(defn xform-matrix
  "Give the matrix corresponding to the given linear transformation.
  Multiplying this matrix by a vector gives the same result as applying
  the transformation to that vector."
  [xfm]
  (let [in-size (count (first (xfm)))]
    (join-cols (map xfm (std-basis in-size)))))

;;; INVERSE OF A MATRIX

(defn- remove-row-column
  "Returns a matrix that contains the same elements as another matrix
  with one row and one column removed."
  [m i j]
  (gen-matrix (dec (:rows m))
              (dec (:cols m))
              #(mget m
                     (if (< % i) % (inc %))
                     (if (< %2 j) %2 (inc %2)))))

(defn cofactor
  "Find a cofactor of an element in a matrix."
  [m i j]
  (let [d (det (remove-row-column m i j))]
    (if (odd? (flattened-index m i j))
      (- d)
      d)))

(defn cofactor-matrix
  "Find the cofactor matrix of the given matrix, used to find the
  inverse of a matrix."
  [m] (gen-matrix (:rows m)
                  (:cols m)
                  (partial cofactor m)))

(defn adjoint
  "Finds an adjoint of the given matrix."
  [m] (transpose (cofactor-matrix m)))

(defn invertible?
  "Checks if a matrix has an inverse."
  [m] (let [d (det m)]
        (if (not= 0 d) d nil)))

;; this so doesn't work on 4x4 matrices or bigger
(defn inverse
  "Finds the inverse of a matrix."
  [m]
  (let [invert (invertible? m)]
    (assert invert)
    (mult (/ 1 invert)
          (adjoint m))))

;;; ELEMENTARY ROW OPERATIONS

(defn swap-rows
  "Swaps two rows of a matrix"
  [m r1 r2]
  (gen-matrix (:rows m)
              (:cols m)
              (fn [i j]
                (mget m
                      (condp = i r1 r2 r2 r1 i)
                      j))))

(defn multiply-row
  "Multiplies a row of a matrix by a scalar"
  [m r n]
  (assert (not= 0 n))
  (gen-matrix (:rows m)
              (:cols m)
              (fn [i j]
                (if (= i r)
                  (* n (mget m i j))
                  (mget m i j)))))

(defn add-mult
  "Adds a multiple of one row to another row"
  [m n r1 r2]
  (gen-matrix (:rows m)
              (:cols m)
              (fn [i j]
                (if (= i r2)
                  (+ (mget m i j)
                     (* n (mget m r1 j)))
                  (mget m i j)))))

;;; ROW REDUCTION
;;; Warning: These have big flaws right now.
;;; They don't handle zeros in the wrong places.
;;; They also will gladly divide by zero if given the chance.

(defn reduce-col [m c done? next]
  (let [mr (multiply-row m c
                         (if (= 0 (mget m c c))
                           1
                           (/ 1 (mget m c c))))]
    (loop [acc mr
           r (next c)]
      (if (done? r)
        acc
        (recur
         (add-mult acc
                   (- (mget acc r c))
                   c
                   r)
         (next r))))))

(defn lower-reduce-column
  "Applies row operations to reduce the lower half of the matrix to
  zeroes. Assumes that the previous columns have already been reduced."
  [m c]
  (reduce-col m c #(> % (:rows m)) inc))

(defn lower-reduce
  "Row-reduces a matrix so it is upper-triangular (reduce the lower half
  to zeroes)."
  [m]
  (reduce lower-reduce-column
          m
          (range 1 (inc (min (:rows m) (:cols m))))))

(defn upper-reduce-column
  "Applies row operations to reduce the upper half of the matrix to
  zeroes. Assumes that subsequent columns have already been reduced."
  [m c]
  (reduce-col m c #(< % 1) dec))

(defn upper-reduce
  "Row-reduces a matrix so it is lower-triangular (reduce the upper half
  to zeroes)."
  [m]
  (reduce upper-reduce-column
          m
          (range (min (:rows m) (:cols m)) 0 -1)))

(defn rref
  "Gives the matrix in reduced row echelon form."
  [m] (upper-reduce (lower-reduce m)))

(defn augment
  "Puts two matrices side-by-side in a new matrix."
  [m1 m2]
  (assert (= (:rows m1) (:rows m2)))
  (gen-matrix (:rows m1)
              (+ (:cols m1) (:cols m2))
              #(if (> %2 (:cols m1))
                 (mget m2 %1 (- %2 (:cols m1)))
                 (mget m1 %1 %2))))

;;; DEALING WITH BASES

(defn in-basis [v bas]
  (let [sol (rref (augment (join-cols bas) v))]
    (col sol (:cols sol))))

(defn change-basis [out in]
  (join-cols (map #(in-basis % out) in)))

;;; NORMALIZING VECTORS

(defn magnitude [v]
  (Math/sqrt (reduce + (map #(* % %) (:data v)))))

(defn norm [v]
  (let [mag (magnitude v)]
    (if (close-enough? 0 mag)
      v
      (mult (/ (magnitude v)) v))))

;;; QR FACTORIZATION

(defn zero-vec? [v]
  (every? (partial close-enough? 0) (:data v)))

(defn orthonormal-basis [m]
  (reduce (fn [vecs v]
            (let [u (norm
                   (apply subt
                          v
                          (map #(mult (dot v %) %) vecs)))]
              (if (zero-vec? u)
                vecs
                (conj vecs u))))
          []
          (cols m)))

;;; PRINTING MATRICES AND VECTORS

(defmulti
  #^{:doc "Prints the individual items in a matrix into 7-character wide
  columns separated by single spaces."}
  print-matrix-item class)

(defmethod print-matrix-item Integer [val]
  (format "%7d " val))

(defmethod print-matrix-item Double [val]
  (format "%7.2f " val))

(defmethod print-matrix-item :default [val]
  (format "%7s " (print-str val)))

(defmacro for-matrix
  "Maps over the indices of a matrix. Returns a Seq, not a matrix."
  [m [i j] body]
  `(for [~i (range 1 (inc (:rows ~m)))
         ~j (range 1 (inc (:cols ~m)))]
     ~body))

(defmacro do-matrix
  "Loops over the indices of a matrix for side effects, like printing."
  [m [i j] & body]
  `(dorun (for-matrix ~m [~i ~j] (do ~@body))))

(defn print-matrix
  "Prints a matrix into columns."
  ([m] (print-matrix m *out*))
  ([m o] (do-matrix m [i j]
           (.write o (print-matrix-item (mget m i j)))
           (if (and (= j (:cols m))
                    (not= i (:rows m)))
             (.write o "\n")))))

;;; This is a disgusting hack to make matrices print special
(defmethod print-method clojure.lang.PersistentStructMap [o #^java.io.Writer w]
  (if (:custom-print ^o)
    ((:custom-print ^o) o w)
    (((methods print-method) clojure.lang.IPersistentMap) o w)))