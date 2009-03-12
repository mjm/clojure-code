;;; BASIC MATH FUNCTIONS
;;;
;;; I'm not sure why clojure doesn't have stuff like this already.
;;; These make some of the other more complicated functions a lot
;;; easier to deal with.

(ns math
  (:use clojure.contrib.math))

(defn math-dispatch
  "Used for multimethod dispatch when working on math data.
  Basically makes it so that if we are working with Clojure data, we
  get the :type field, and if we are working with Java data, we use
  its class. The only exception is Ratio, which doesn't fit into
  Java's numeric hierarchy. If any element is a Ratio, we return
  Number so it is treated like others."
  ([a] (or (:type a)
           (if (= clojure.lang.Ratio (class a))
             Number
             (class a))))
  ([a & args]
     (vec (map math-dispatch (cons a args)))))

(defn avg
  "Returns the average of the given numbers."
  [& nums]
  (/ (reduce + nums)
     (count nums)))

;; Whereas Math/pow always uses doubles, this tries to preserve the
;; type of the input if possible (if the exponent is an integer).
(defn **
  "Raises base to the exp power."
  [base exp]
  (if (and (integer? exp) (> exp -1))
    (reduce * (replicate exp base))
    (Math/pow base exp)))

(def #^{:private true}
     close-enough-delta
     0.000000001)

(defmulti =?
  "Checks if two numbers are close enough to be considered equal. This
  is meant to avoid some problems with bad floating-point arithmetic."
  math-dispatch)

(defmethod =? [Number Number] [n1 n2]
  (< (abs (- n1 n2)) close-enough-delta))
