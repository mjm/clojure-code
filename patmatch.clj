;;; This is pretty much a straight port from PAIP

(ns patmatch
  (:use clojure.contrib.test-is))

(def fail nil)
(def match)

(with-test
    (defn match-variable [var in bindings]
      (let [b (bindings var)]
        (cond (nil? b) (assoc bindings var in)
              (= in b) bindings
              :else fail)))
  (is (= '{?x 1} (match-variable '?x 1 {})))
  (is (= '{?x 1} (match-variable '?x 1 '{?x 1})))
  (is (= fail (match-variable '?x 2 '{?x 1})))
  (is (= '{?x 1 ?y 2} (match-variable '?x 1 '{?y 2}))))

(with-test
    (defn variable? [x]
      (and (symbol? x)
           (.startsWith (name x) "?")))
  (is (variable? '?x))
  (is (variable? '?y))
  (is (variable? '?whatev))
  (is (not (variable? 'x)))
  (is (not (variable? 'whatev))))

(defmacro with-vars [vars & body]
  `(binding [variable? (fn [x#] (some #{x#} '~vars))]
     ~@body))

(defn match-is [[var pred] in bindings]
  (let [new-bindings (match var in bindings)]
    (if (or (= new-bindings fail)
            (not ((resolve pred) in)))
      fail
      new-bindings)))

(defn match-or [pats in bindings]
  (if (nil? pats)
    fail
    (let [new-bindings (match (first pats) in bindings)]
      (if (= new-bindings fail)
        (match-or (rest pats) in bindings)
        new-bindings))))

(defn match-and [pats in bindings]
  (cond (= bindings fail) fail
        (nil? pats) bindings
        :else (match-and (rest pats)
                         in
                         (match (first pats)
                                in
                                bindings))))

(defn match-not [pats in bindings]
  (if (match-or pats in bindings)
    fail
    bindings))

(def single-matchers
     {'?is match-is
      '?or match-or
      '?and match-and
      '?not match-not})

(defn single-pattern? [pat]
  (and (coll? pat)
       (single-matchers (first pat))))

(defn single-match [pat in bindings]
  ((single-matchers (first pat))
   (rest pat) in bindings))

(defn match
  ([pat exp] (match pat exp {}))
  ([pat exp bindings]
     ;;(prn (str pat " = " exp " " bindings))
     (cond (= bindings fail) fail
           (variable? pat) (match-variable pat exp bindings)
           (and (not (coll? pat))
                (not (coll? exp))
                (= pat exp))
           bindings
           (single-pattern? pat) (single-match pat exp bindings)
           (and (coll? pat) (coll? exp))
           (recur (rest pat) (rest exp)
                  (match (first pat) (first exp) bindings))
           :else fail)))

(defn expand-abbrevs [pat abbrevs]
  (if-let [exp (abbrevs pat)]
    exp
    (if (coll? pat)
      (cons (expand-abbrevs (first pat) abbrevs)
            (expand-abbrevs (rest pat) abbrevs))
      pat)))

(defn substitute [exp bindings]
  (or (bindings exp)
      (if (coll? exp)
        (cons (substitute (first exp) bindings)
              (substitute (rest exp) bindings))
        exp)))

(defn translate-by-rules [exp rules actfn]
  (some #(if-let [res (match (first %) exp)]
           (actfn (second %) res))
        rules))

(run-tests)