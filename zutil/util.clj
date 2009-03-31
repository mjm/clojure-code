(ns zutil.util
  (:import (java.io File)))

(defn position
  ([x coll] (position x coll 0))
  ([x coll n]
     (cond (nil? (first coll)) nil
           (= x (first coll )) n
           true (recur x (drop 1 coll) (inc n)))))

(defn rposition [x coll]
  (let [len (count coll)
        pos (position x (reverse coll))]
    (when pos (- len pos))))

(defn member? [x coll]
  (when (filter #(= % x) coll)
    x))


(defn append-pathname [pathname x]
  (let [sep (rposition \. pathname)]
  (str (subs pathname 0 (dec sep))
       x
       \.
       (subs pathname sep))))

(defn unique-filename
  ([pathname] (unique-filename pathname pathname 1))
  ([pathname try-path n]
     (if (.exists (File. try-path))
       (recur pathname (append-pathname pathname (str \- n)) (inc n))
       try-path)))

(defn identity* [x y]
  (identity y))