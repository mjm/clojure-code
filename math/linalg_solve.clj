;;; Operations for solving linear systems of equations using matrices

(ns math.linalg)

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
