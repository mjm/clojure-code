;;; BASIC MATH FUNCTIONS
;;;
;;; I'm not sure why clojure doesn't have stuff like this already.
;;; These make some of the other more complicated functions a lot
;;; easier to deal with.

(in-ns 'math.linalg)

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

;; Wrote our own because Math/abs doesn't work with Ratios
(defn abs
  "Returns the absolute value of the given number."
  [n] (if (< n 0) (- n) n))

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
  (if (integer? exp)
    (reduce * (replicate exp base))
    (Math/pow base exp)))

(defn- =?
  "Checks if two numbers are close enough to be considered equal. This
  is meant to avoid some problems with bad floating-point arithmetic."
  [n1 n2]
  (< (abs (- n1 n2)) 0.000000001))

;; This shit is crappy! Real ugly results for what should be integers.
;; Idea: use =? to check if it really is close enough to a rounded
;; answer. If so, do the rounding and return the integer.

;; (defn- is-valid? [guess n]
;;   (< (abs (- (** guess 2) n))
;;      0.000000001))

;; (defn- improve [guess n]
;;   (avg guess (/ n guess)))

;; (defn sqrt [num]
;;   (loop [guess 1
;;          n num]
;;     (if (is-valid? guess n)
;;       guess
;;       (recur (improve guess n) n))))