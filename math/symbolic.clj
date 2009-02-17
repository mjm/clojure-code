(ns math.symbolic
  (:use math
        clojure.contrib.test-is))

(def simp-rules
     '(((+ x 0) x)
       ((+ 0 x) x)
       ((+ x x) (* 2 x))
       ((- x 0) x)
       ((- 0 x) (- x))
       ((- x x) 0)))

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
  (if (can-eval? exp)
    (eval exp)
    exp))

(defn simplify [exp]
  (if (coll? exp)
    (simplify-exp (map simplify exp))
    exp))
