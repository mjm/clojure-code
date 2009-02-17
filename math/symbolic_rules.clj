(in-ns 'math.symbolic)

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
       (+ x (- y x)) y
       (+ (* y x) (* z x)) (* (+ y z) x)
       (log 1) 0
       (log e) 1
       (sin 0) 0
       (sin pi) 0
       (cos 0) 1
       (cos pi) -1
       (sin (/ pi 2)) 1
       (cos (/ pi 2)) 0
       (log (** e x)) x
       (** e (log x)) x
       (* (** x y) (** x z)) (** x (+ y z))
       (/ (** x y) (** x z)) (** x (- y z))
       (+ (log x) (log y)) (log (* x y))
       (- (log x) (log y)) (log (/ x y))
       (+ (** (sin x) 2) (** (cos x) 2)) 1})