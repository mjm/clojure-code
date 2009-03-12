;;; Operations for solving linear systems of equations using matrices

(ns math.linalg)

;;; ELEMENTARY ROW OPERATIONS

(in-ns 'math.linalg)

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

;;; GAUSS ELIMINATION

(defn partial-pivot
  "Swaps rows to make the pivot column of the matrix be as large as
  possible."
  [A pivot-col]
  (swap-rows A pivot-col
             (apply max-key
                    #(abs (mget A % pivot-col))
                    (range pivot-col (inc (:rows A))))))

(defn eliminate-column
  "Turns a column of a matrix into zeros. Takes a range to indicate
  which rows to eliminate the data from."
  [A column rows]
  (reduce #(add-mult %1
                     (- (mget %1 %2 column))
                     column
                     %2)
          A
          rows))

(defn gauss-eliminate-no-partial
  "Perform elimination of a column without the use of partial pivoting."
  [A pivot-col]
  (let [pivot (mget A pivot-col pivot-col)]
    (if (=? pivot 0) ; No elimination can be done in this column
      A
      (let [A* (multiply-row A pivot-col
                             (/ pivot))] ; Make the pivot 1
        (eliminate-column A* pivot-col
                          (range (inc pivot-col)
                                 (inc (:rows A*))))))))

(defn gauss-eliminate [A pivot-col]
  "Performs elimination on a single column. Uses partial pivoting to
  reduce errors. Should handle zeros just fine."
  (let [A* (partial-pivot A pivot-col)]
    (gauss-eliminate-no-partial A* pivot-col)))

(defn back-substitute [A]
  "Clear the upper portion of the matrix using row operations."
  (let [n (min (:rows A) (:cols A))
        r (range n 1 -1)]
    (reduce (fn [A pivot-col]
              (let [pivot (mget A pivot-col pivot-col)]
                (if (=? pivot 0)
                  A
                  (eliminate-column A pivot-col
                                    (range (dec pivot-col) 0 -1)))))
            A r)))

(defn gauss-solve
  "Given a function used to perform row-reduction, create a function
  that uses that elimination function to solve an equation A x = b. The
  function passed should reduce the matrix to be upper triangular."
  [eliminate]
  (fn [A b]
    (let [A (augment A b)
          n (min (:rows A) (:cols A))
          r (range 1 (inc n))]
      (col (back-substitute (reduce eliminate A r))
           (:cols A)))))

(defvar #^{:arglists '([A b])}
  gauss
  (gauss-solve gauss-eliminate-no-partial)
  "Solves an equation using Gaussian elimination without partial
  pivoting.")

(defvar #^{:arglists '([A b])}
  gauss-partial
  (gauss-solve gauss-eliminate)
  "Solves an equation using Gaussian elimination using partial
  pivoting.")

(declare norm)

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

(defn qr-gram-schmidt
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

(defn iter-solve
  "Creates a function that will perform an iterative method for solving
  A x = b. Uses an iteration function that will transform an x vector
  into a better approximation."
  [iter]
  (fn [A b]
    (loop [x (zero (:rows b) (:cols b))]
      (let [x1 (iter A b x)]
        (if (=? x x1)
          x1
          (recur x1))))))

(defn- iter-sigma
  "Finds the sigma value used in a single row iteration of one of the
  iterative methods."
  [A x i]
  (reduce +
          (map #(mult (mget A i %) (mget x %))
               (concat (range 1 i)
                       (range (inc i) (inc (dim x)))))))

(defn- gs-iterate
  "Iterates over the x vector once to create a new x vector that is a
  closer approximation of the true x vector."
  [A b x]
  (loop [x x
         i 1]
    (if (> i (dim x))
      x
      (recur (massoc x i 1
                     (/ (subt (mget b i) (iter-sigma A x i))
                        (mget A i i)))
             (inc i)))))

(def
 #^{:doc "Uses Gauss-Seidel iteration to approximate the solution
  to A x = b."}
 gauss-seidel (iter-solve gs-iterate))

(defn- jac-iterate
  "Iterates over the x vector once to create a new x vector that is a
  closer approximation to the true x vector."
  [A b x]
  (gen-matrix (:rows x) (:cols x)
              (fn [i _]
                (/ (subt (mget b i) (iter-sigma A x i))
                   (mget A i i)))))

(def
 #^{:doc "Uses Jacobi iteration to approximate a solution to
  to A x = b."}
 jacobi (iter-solve jac-iterate))
