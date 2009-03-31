(ns zutil.csv
  (:use zutil.util)
  (:import (java.util.regex Pattern)
           (java.io FileReader FileWriter BufferedReader File)))

;; regexeps never get it right
;; (defn line->vec [line]
;;   (map read-string
;;        (vec (. (. Pattern (compile ",(?=(?:[^\"]*\"[^\"]*\")*(?![^\"]*\"))"))
;;                (split line)))))

(defn csv->seq [f]
  (with-open [reader (BufferedReader. (FileReader. f))]
    (doall (line-seq reader))))
        

(defn quote-if-string [x]
  (if (and (string? x) (not (empty? x)))
    (str \" x \")
    x))

(defn struct->csv [s]
  (apply str 
         (concat (interpose \, (map quote-if-string (vals s)))
                 [\return \newline])))

(defn seq->csv [s]
  (apply str (concat (interpose \, (map quote-if-string s))
                     [\return \newline])))

(defn write-csv [f v]
  (doto (FileWriter. f)
    (.write (apply str (map seq->csv v)))
    (.close)))

(defn scan-for-quote [s]
  (position \" s))
  (comment (let [pos (position \" s)]
    (unless (and pos (not (= \\ (nth s (dec pos)))))
            pos)))

(defn scan-for-delimiter
  ([s] (scan-for-delimiter s 0 false))
  ([s start quote]
     (let [rest (drop start s)
           qpos (scan-for-quote rest)
           pos (position \, rest)]
       (cond 
        quote (recur s (+ start 1 (scan-for-quote rest)) false) 
        (and qpos pos (> pos qpos)) (recur s (+ start 1 qpos) true)
        pos (+ start pos)))))

(defn line->list
  ([s] (line->list s []))
  ([s l]
     (let [i (scan-for-delimiter s)]
       (if (nil? i)
         (doall (map #(if (empty? %)
                        ""
                        (try (read-string %) ;fails with whitespace
                             (catch RuntimeException e
                               "")))
                     (reverse (cons (apply str s) l))))
         (recur (drop (inc i) s)
                (cons (apply str (take i s)) l))))))

