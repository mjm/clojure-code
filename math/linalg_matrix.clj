(in-ns 'math.linalg)

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

(defn zero-vec? [v]
  (every? (partial =? 0) (:data v)))