(ns math.symbolic
  (:use math
        patmatch
        clojure.contrib.test-is))

(load "symbolic_rules")

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
  `(with-vars ~(map identity '[x y z m n o p q r s t u v w])
     ~@body))

(defn simp [exp]
  (math (simplify exp)))

(defn subst [exp vals]
  (math (simplify (substitute exp vals))))