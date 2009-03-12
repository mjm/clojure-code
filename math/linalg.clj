(ns math.linalg
  (:use math
        clojure.contrib.math
        clojure.contrib.def
        clojure.contrib.test-is))

(load "linalg_matrix")                  ; Matrix and vector definition
(load "linalg_arith")                   ; Matrix arithmetic
(load "linalg_solve")                   ; Solving systems of equations
(load "linalg_ops")                     ; Matrix and vector operations
(load "linalg_xform")                   ; Linear transformations
(load "linalg_print")                   ; Printing matrices to the REPL
