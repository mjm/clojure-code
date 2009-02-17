(ns math.symbolic
  (:use math
        patmatch
        clojure.contrib.test-is))

(def simp-rules
     '{(+ x 0) x
       (+ 0 x) x
       (+ x x) (* 2 x)
       (- x 0) x
       (- 0 x) (- x)
       (- x x) 0
       (- (- x)) x
       (* x 1) x
       (* 1 x) x
       (* x 0) 0
       (* 0 x) 0
       (* x x) (** x 2)
       (/ 0 x) 0
       (/ x 1) x
       (/ x x) 1
       (** x 0) 1
       (** 0 x) 0
       (** 1 x) 1
       (** x 1) x
       (** x -1) (/ 1 x)
       (* x (/ y x)) y
       (* (/ y x) x) y
       (/ (* y x) x) y
       (/ (* x y) x) y
       (+ x (- x)) 0
       (+ x (- y x)) y})

(defn- can-eval-op?
  [op]
  (some #{op} '(+ - * / **)))

(defn- can-eval-args?
  [args]
  (every? number? args))

(with-test
    (defn can-eval?
      [exp]
      (and (can-eval-op? (first exp))
           (can-eval-args? (rest exp))))
  (is (can-eval? '(+ 1 2)))
  (is (not (can-eval? '(+ a 3))))
  (is (not (can-eval? '(+ 3 b))))
  (is (not (can-eval? '(+ a b)))))

(defn simplify-exp
  [exp]
  (or (translate-by-rules exp simp-rules #(simplify (substitute % %2)))
      (if (can-eval? exp) (eval exp)
          exp)))

(defn simplify [exp]
  (if (coll? exp)
    (simplify-exp (map simplify exp))
    exp))

(defmacro math [& body]
  `(with-vars [x y z m n o p q r s t u v w]
     ~@body))
