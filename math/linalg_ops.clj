;;; STANDARD MATRIX AND VECTOR OPERATIONS
;;;
;;; Defines many common operations for matrices and vectors.
;;;
;;; Operations:
;;;   transpose: Transpose a matrix
;;;   dot: Dot product of two vectors
;;;   id: Identity matrix
;;;   zero: Matrix of zeros
;;;   det: Determinant
;;;   join-cols: Matrix from a group of vectors
;;;   adjoint: Adjoint of a matrix
;;;   inverse: Invert a matrix (only works on 3x3 ATM)
;;;   augment: Place matrices side-by-side
;;;   solve: Use row-reduction to solve Ax = b for x
;;;   magnitude: Length of a vector
;;;   norm: Normalize a vector
;;;   in-basis: Rewrites a coordinate vector in terms of a basis
;;;   change-basis: Change of basis matrix
;;;   orthonormal-basis: Finds an orthonormal basis for the image
;;;   qr: QR factorization
;;;   ls-solve: Least-squares solution to Ax = b
;;;   poly-regression: Polynomial regression of any degree
;;;   linear-regression: Polynomial regression of degree 1

(in-ns 'math.linalg)

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
      "Take the dot product of two vectors."
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
    (defn id
      "Gives an identity matrix with n rows and n columns."
      [n] (gen-matrix n n #(if (= %1 %2) 1 0)))
  (is (= (id 1) (matr 1 1 [1])))
  (is (= (id 2) (matr 2 2 [1 0 0 1])))
  (is (= (id 3) (matr 3 3 [1 0 0 0 1 0 0 0 1]))))

(defn zero
  "Gives a matrix with m rows and n columns filled with zeroes."
  ([m] (zero m m))
  ([m n] (gen-matrix m n (constantly 0))))

(defmulti det
  "Take the determinant of a matrix."
  (fn [m]
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

(defn- join-cols
  "Take a sequence of vectors and forms a matrix with those vectors as
  the columns."
  [col-seq]
  (matr (dim (first col-seq))
        (count col-seq)
        (apply interleave (map :data col-seq))))

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

(defn augment
  "Puts two matrices side-by-side in a new matrix."
  [m1 m2]
  (assert (= (:rows m1) (:rows m2)))
  (gen-matrix (:rows m1)
              (+ (:cols m1) (:cols m2))
              #(if (> %2 (:cols m1))
                 (mget m2 %1 (- %2 (:cols m1)))
                 (mget m1 %1 %2))))

(defn solve
  "Solves an equation of the form Ax = b, where A is a matrix and b is
  a column vector."
  [a b]
  (let [sol (rref (augment a b))]
    (col sol (:cols sol))))

(defn magnitude
  "Returns the length of the vector."
  [v]
  (sqrt (reduce + (map #(* % %) (:data v)))))

(defn norm
  "Normalizes the vector by dividing by the magnitude of the vector.
  Should yield a unit vector with the same direction as the original
  vector."
  [v]
  (let [mag (magnitude v)]
    (if (=? 0 mag)
      v
      (mult (/ (magnitude v)) v))))

(defn in-basis
  "Takes a coordinate vector and writes it in terms of the given basis."
  [v bas]
  (solve (join-cols bas) v))

(defn change-basis
  "Produces a matrix for a transformation that will take a vector in the
  in basis and produce the same vector written in the out basis."
  [out in]
  (join-cols (map #(in-basis % out) in)))

(defn orthonormal-basis
  "Returns a sequence of column vectors that form an orthonormal basis
  for the image of the given matrix. Uses the Gram-Schmidt procedure to
  produce the basis."
  [m]
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

(defn q-matrix
  "Returns a matrix whose columns are the vectors of an orthonormal
  basis for the image of the matrix."
  [m] (reduce augment (orthonormal-basis m)))

(defn r-matrix
  "Returns a matrix that, when multiplied with the Q matrix, produces A"
  [a q] (mult (transpose q) a))

(defn qr
  "Returns a vector with both the Q and R matrices for the given
  matrix."
  [a] (let [q (q-matrix a)
            r (r-matrix a q)]
        [q r]))

(defn hh-vector
  "Transforms a vector to a multiple of e1 while preserving length. Used
  to compute a Householder matrix."
  [a]
  (let [val (mget a 1)]
    ((if (pos? val) add subt)
     a
     (apply cvec
            (magnitude a)
            (replicate (dec (dim a)) 0)))))

(defn householder
  "Computes the Householder matrix for a given vector."
  [a]
  (let [u (norm (hh-vector a))
        ut (transpose u)]
    (subt (id (dim u))
          (mult 2 u ut))))

(defn- hh-pad
  "Pads the upper-left of a matrix so that it has the same size as other
  matrices and preserves the values that already have."
  [m d]
  (assert (square? m))
  (assoc-sub-matrix (id (+ (:rows m) d))
                    m
                    (inc d)
                    (inc d)))

(defn hh-reduce
  "Use Householder matrices to reduce A to upper triangular. Returns a
  vector containing the upper triangular matrix R and the sequence of
  Householder matrices used to get R."
  [A]
  (loop [A* A
         Hs []
         num 1]
    (if (or (= num (:rows A*))
            (= num (:cols A*)))
      [A* Hs]
      (let [Asub (sub-matrix A* num num)
            a (col Asub 1)
            H* (hh-pad (householder a) (dec num))]
        (recur (mult H* A*)
               (conj Hs H*)
               (inc num))))))

(defn qr-householder
  "Performs a QR-factorization using Householder matrices. "
  [A]
  (let [[R Hs] (hh-reduce A)]
    [(apply mult Hs) R]))

(defn ls-solve
  "Finds a least squared solution to the equation Ax = b. Uses the QR
  factorization for better computations."
  [a b]
  (let [[q r] (qr a)]
    (solve r (mult (transpose q) b))))

(defn- make-poly-matrix
  "Creates the matrix used in solving a polynomial regression."
  [order xs]
  (matr (count xs)
        (inc order)
        (apply concat
               (map (fn [x]
                      (map (partial ** x)
                           (range order -1 -1)))
                    xs))))

(defn poly-regression
  "Finds a polynomial regression of given order on the data points."
  [order points]
  (ls-solve (make-poly-matrix order
                              (map first points))
            (apply cvec (map second points))))

(def linear-regression (partial poly-regression 1))

(defn eigenvalues
  "Finds a column vector of the eigenvalues of the matrix. Uses the QR
  method."
  [m]
  (let [z? (=? 0 (det m))]
    (loop [m m]
      (let [[q r] (qr m)
            m1 (mult r q)
            d0 (diag m)
            d1 (diag m1)]
        (if (every? (fn [[a b]] (=? a b))
                    (zipmap d0 d1))
          (if z?
            (apply cvec 0 d1)
            (apply cvec d1))
          (recur m1))))))

;; (defn eigenvectors
;;   "Finds the eigenvectors that correspond to the given eigenvalues of
;;   the matrix."
;;   [m eigs]
;;   (join-cols
;;    (map #(eigenvector m %)
;;         (:data eigs))))