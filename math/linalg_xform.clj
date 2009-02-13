;;; LINEAR TRANSFORMATIONS
;;;
;;; Provides support for defining Clojure functions that represent
;;; linear transformations. These functions are defined using a
;;; similar syntax to fn, except they automatically bind the arguments
;;; to the components of the input vector and, when called with no
;;; arguments, they return a representation of the source which is
;;; used for printing.

(in-ns 'math.linalg)

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

(defn xform-matrix
  "Give the matrix corresponding to the given linear transformation.
  Multiplying this matrix by a vector gives the same result as applying
  the transformation to that vector."
  [xfm]
  (let [in-size (count (first (xfm)))]
    (join-cols (map xfm (std-basis in-size)))))