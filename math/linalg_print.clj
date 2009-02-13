;;; PRINTING MATRICES AND VECTORS

(in-ns 'math.linalg)

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