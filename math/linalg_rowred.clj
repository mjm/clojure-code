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

;;; ROW REDUCTION
;;; Warning: These have big flaws right now.
;;; They don't handle zeros in the wrong places.
;;; They also will gladly divide by zero if given the chance.

(defn- clean-for-reduction [m]
  (if (=? (mget m 1 1) 0)
    (swap-rows m 1 (max-key #(mget m % 1) (range 1 (inc (:rows m)))))
    m))

(defn- reduce-col [m c done? next]
  (let [mr (multiply-row m c
                         (if (=? 0 (mget m c c))
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

(defn- lower-reduce-column
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

(defn- upper-reduce-column
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