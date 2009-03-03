(ns math.symbolic
  (:refer-clojure :rename {+ old+, - old-, * old*}
                  :exclude [/])
  (:use [math :rename {** old**}]
        [patmatch :only [substitute]])
  (:use clojure.contrib.test-is
        clojure.contrib.seq-utils)
  (:require [clojure.contrib.error-kit :as err]))

(in-ns 'math.symbolic)

(defmacro sym [var]
  `(def ~var '~var))

(sym pi)
(sym e)

(with-test
    (def variable? symbol?)
  (is (variable? 'x))
  (is (variable? 'y))
  (is (not (variable? 1)))
  (is (not (variable? '(+ 1 2)))))

(with-test
    (defn same-variable? [v1 v2]
      (and (variable? v1)
           (variable? v2)
           (= v1 v2)))
  (is (same-variable? 'x 'x))
  (is (same-variable? 'y 'y))
  (is (not (same-variable? 'x 'y)))
  (is (not (same-variable? 1 1)))
  (is (not (same-variable? 1 2))))

(def +)

(with-test
    (def first-term second)
  (is (= 2 (first-term '(+ 2 x))))
  (is (= 'y (first-term '(+ y x)))))

(with-test
    (defn second-term [exp]
      (nth exp 2))
  (is (= 2 (second-term '(+ x 2))))
  (is (= 'y (second-term '(+ x y)))))

(with-test
    (defn sum? [val]
      (and (coll? val)
           (= (first val) '+)))
  (is (sum? '(+ 1 2)))
  (is (sum? '(+ x 2)))
  (is (sum? (+ 'x 2)))
  (is (not (sum? 1)))
  (is (not (sum? 'x))))

(with-test
    (defn +
      ([] 0)
      ([x] x)
      ([x y]
         (cond (and (number? x) (not (number? y))) (+ y x)
               (and (sum? x)
                    (number? (second-term x))
                    (number? y)) (+ (first-term x)
                                    (+ (second-term x) y))
                    (= 0 x) y
                    (= 0 y) x
                    (and (number? x) (number? y)) (old+ x y)
                    :else (list '+ x y)))
      ([x y & more]
         (reduce + (+ x y) more)))
  (is (= 3 (+ 1 2)))
  (is (= '(+ x 2) (+ 'x 2)))
  (is (= '(+ x 2) (+ 2 'x)))
  (is (= 6 (+ 1 2 3)))
  (is (= '(+ x 4) (+ 2 'x 2)))
  (is (= '(+ x 4) (+ 2 2 'x))))

(def -)

(with-test
    (defn negated? [val]
      (and (coll? val)
           (= (first val) '-)))
  (is (negated? '(- x)))
  (is (negated? '(- (+ x 2))))
  (is (not (negated? -2)))
  (is (not (negated? 'x))))

(with-test
    (defn -
      ([x] (if (number? x) (old- x) (list '- x)))
      ([x y] (+ x (- y)))
      ([x y & more]
         (reduce - (- x y) more)))
  (is (= '(- x) (- 'x)))
  (is (= '(+ x (- y)) (- 'x 'y)))
  (is (= -2 (- 2)))
  (is (= 0 (- 3 2 1))))

(def *)

(with-test
    (defn product? [val]
      (and (coll? val)
           (= (first val) '*)))
  (is (product? '(* 2 x)))
  (is (product? '(* 2 3)))
  (is (product? (* 'x 2)))
  (is (not (product? '(+ x 2))))
  (is (not (product? 2)))
  (is (not (product? 'x))))

(with-test
    (defn *
      ([] 1)
      ([x] x)
      ([x y]
         (cond (or (= 0 x) (= 0 y)) 0
               (= 1 x) y
               (= 1 y) x
               (and (number? x)
                    (product? y)
                    (number? (first-term y)))
               (* (* x (first-term y)) (second-term y))
               (and (not (number? x)) (number? y)) (* y x)
               (and (number? x) (number? y)) (old* x y)
               :else (list '* x y)))
      ([x y & more]
         (reduce *
                 (* x y)
                 more)))
  (is (= 6 (* 2 3)))
  (is (= '(* 2 x) (* 2 'x)))
  (is (= '(* 2 x) (* 'x 2))))

(def div)

(with-test
    (defn quotient? [val]
      (and (coll? val)
           (= (first val) '/)))
  (is (quotient? '(/ 2 x)))
  (is (quotient? '(/ x 2))))

(with-test
    (def dividend first-term)
  (is (= 2 (dividend '(/ 2 x))))
  (is (= 'x (dividend '(/ x 2)))))

(with-test
    (def divisor second-term)
  (is (= 'x (divisor '(/ 2 x))))
  (is (= 2 (divisor '(/ x 2)))))

(defonce old-div clojure.core//)

(defn div
  ([x] (div 1 x))
  ([x y] (cond (= y 0) Double/POSITIVE_INFINITY
               (= x 0) 0
               (= y 1) x
               (= x y) 1
               (and (number? x) (number? y)) (old-div x y)
               :else (list '/ x y)))
  ([x y & more]
     (reduce div (div x y) more)))

;;; <evil thanks="durka42">
(defonce ___do-stuff
  (do (ns-unmap 'math.symbolic '/)
      (.refer *ns* (symbol nil "/") #'div)))
;;; </evil>

(with-test
    (defn exponent? [val]
      (and (coll? val)
           (= (first val) '**)))
  (is (exponent? '(** x 2)))
  (is (exponent? '(** 2 n)))
  (is (not (exponent? 2)))
  (is (not (exponent? 'n)))
  (is (not (exponent? '(+ 1 2))))
  (is (not (exponent? '(* 1 2)))))

(with-test
    (def base first-term)
  (is (= 'x (base '(** x 2))))
  (is (= 2 (base '(** 2 n)))))

(with-test
    (def exponent second-term)
  (is (= 2 (exponent '(** x 2))))
  (is (= 'n (exponent '(** 2 n)))))

(with-test
    (defn ** [b e]
      (cond (= 0 e) 1
            (= 1 e) b
            (and (number? b) (number? e)) (old** b e)
            :else (list '** b e)))
  (is (= 1 (** 'x 0)))
  (is (= 'x (** 'x 1)))
  (is (= 8 (** 2 3)))
  (is (= '(** x 2) (** 'x 2))))

(err/deferror *derivative-error* []
              [exp]
              {:msg (str "Error trying to derive: " exp)
               :unhandled (err/throw-msg Exception)})

(def deriv)

(with-test
    (defn deriv-sum [exp var]
      (+ (deriv (first-term exp) var)
         (deriv (second-term exp) var)))
  (is (= 1 (deriv-sum '(+ x 3) 'x))))

(with-test
    (defn deriv-product [exp var]
      (+ (* (first-term exp)
            (deriv (second-term exp) var))
         (* (second-term exp)
            (deriv (first-term exp) var))))
  (is (= 'y (deriv-product '(* x y) 'x)))
  (is (= 2 (deriv-product '(* 2 x) 'x))))

(with-test
    (defn power-rule? [exp var]
      (and (exponent? exp)
           (number? (exponent exp))))
  (is (power-rule? (** 'x 2) 'x))
  (is (power-rule? (** 'y 5) 'y))
  (is (not (power-rule? (** 2 'x) 'x)))
  (is (not (power-rule? (* 2 (** 'x 2)) 'x))))

(with-test
    (defn deriv-power [exp var]
      (* (exponent exp)
         (** (base exp) (dec (exponent exp)))
         (deriv (base exp) var)))
  (is (= '(* 2 x) (deriv-power (** 'x 2) 'x))))

(def deriv-rules
     [(fn [n _] (number? n)) (constantly 0)
      (fn [v _] (variable? v)) #(if (same-variable? %1 %2) 1 0)
      (fn [s _] (sum? s)) deriv-sum
      (fn [p _] (product? p)) deriv-product
      power-rule? deriv-power
      (constantly true) (fn [e _] (err/raise *derivative-error* e))])

(with-test
    (defn deriv [exp var]
      (loop [[r & rules] (partition 2 deriv-rules)]
        (if ((first r) exp var)
          ((second r) exp var)
          (recur rules))))
  (is (= 0 (deriv 2 'x)))
  (is (= 1 (deriv 'x 'x)))
  (is (= 0 (deriv 'y 'x)))
  (is (= 1 (deriv '(+ x 3) 'x)))
  (is (= 'y (deriv '(* x y) 'x)))
  (is (= '(+ (* x y) (* (+ x 3) y)) (deriv '(* (* x y) (+ x 3)) 'x))))

(run-tests)