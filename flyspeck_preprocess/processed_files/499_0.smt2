(set-logic QF_NRA)
(declare-fun x1 () Real)
(declare-fun x2 () Real)
(declare-fun x3 () Real)
(declare-fun x4 () Real)
(declare-fun x5 () Real)
(declare-fun x6 () Real)
(assert (<= 2.0 x1))
(assert (<= x1 2.0))
(assert (<= 2.0 x2))
(assert (<= x2 2.0))
(assert (<= 2.0 x3))
(assert (<= x3 2.0))
(assert (<= 6.06887582536 x4))
(assert (<= x4 6.3504))
(assert (<= 4.0 x5))
(assert (<= x5 6.06887582536))
(assert (<= 4.0 x6))
(assert (<= x6 6.06887582536))
(assert (! (not (< (+ (* (* (/ (^ (* 4.0 x4) 0.5) (+ (- (* (- x2) x6) (* x4 x1)) (+ (* x2 x5) (+ (- (* x6 x3) (* x5 x3)) (* x4 (+ (- x4) (+ x2 (+ (- x6 x1) (+ x5 x3))))))))) (marctan (/ (* 4.0 (* x4 (+ (* x4 (* x1 (+ (- x4) (+ x2 (+ (- x6 x1) (+ x5 x3)))))) (+ (* x2 (* x5 (+ (- x4 x2) (+ x6 (+ (- x1 x5) x3))))) (- (- (- (- (* x6 (* x3 (+ x4 (+ (- x2 x6) (+ x1 (- x5 x3)))))) (* x2 (* x6 x1))) (* x4 (* x6 x5))) (* x4 (* x2 x3))) (* x1 (* x5 x3))))))) (^ (+ (- (* (- x2) x6) (* x4 x1)) (+ (* x2 x5) (+ (- (* x6 x3) (* x5 x3)) (* x4 (+ (- x4) (+ x2 (+ (- x6 x1) (+ x5 x3)))))))) 2.0)))) 0.008) (* (- (/ 1.0 12.0) (- (* (+ (/ (marctan (/ (+ (* x5 (* 2.0 (+ (- x5) (+ 2.0 (+ (- x4 2.0) (+ x6 2.0)))))) (+ (* 2.0 (* x6 (+ (- x5 2.0) (+ x4 (+ (- 2.0 x6) 2.0))))) (- (- (- (- (* x4 (* 2.0 (+ x5 (+ (- 2.0 x4) (+ 2.0 (- x6 2.0)))))) (* 2.0 (* x4 2.0))) (* x5 (* x4 x6))) (* x5 (^ 2.0 2.0))) (* 2.0 (* x6 2.0))))) (* 4.0 (^ (+ (^ (* x5 (* 2.0 x4)) 0.5) (+ (* (^ x5 0.5) (/ (+ 2.0 (- x4 2.0)) 2.0)) (+ (* (^ 2.0 0.5) (/ (+ x5 (- x4 x6)) 2.0)) (* (^ x4 0.5) (/ (+ x5 (- 2.0 2.0)) 2.0))))) 2.0)))) (+ (^ (* x5 (* 2.0 x4)) 0.5) (+ (* (^ x5 0.5) (/ (+ 2.0 (- x4 2.0)) 2.0)) (+ (* (^ 2.0 0.5) (/ (+ x5 (- x4 x6)) 2.0)) (* (^ x4 0.5) (/ (+ x5 (- 2.0 2.0)) 2.0)))))) (+ (/ (marctan (/ (+ (* x6 (* 2.0 (+ (- x6) (+ 2.0 (+ (- x5 2.0) (+ x4 2.0)))))) (+ (* 2.0 (* x4 (+ (- x6 2.0) (+ x5 (+ (- 2.0 x4) 2.0))))) (- (- (- (- (* x5 (* 2.0 (+ x6 (+ (- 2.0 x5) (+ 2.0 (- x4 2.0)))))) (* 2.0 (* x5 2.0))) (* x6 (* x5 x4))) (* x6 (^ 2.0 2.0))) (* 2.0 (* x4 2.0))))) (* 4.0 (^ (+ (^ (* x6 (* 2.0 x5)) 0.5) (+ (* (^ x6 0.5) (/ (+ 2.0 (- x5 2.0)) 2.0)) (+ (* (^ 2.0 0.5) (/ (+ x6 (- x5 x4)) 2.0)) (* (^ x5 0.5) (/ (+ x6 (- 2.0 2.0)) 2.0))))) 2.0)))) (+ (^ (* x6 (* 2.0 x5)) 0.5) (+ (* (^ x6 0.5) (/ (+ 2.0 (- x5 2.0)) 2.0)) (+ (* (^ 2.0 0.5) (/ (+ x6 (- x5 x4)) 2.0)) (* (^ x5 0.5) (/ (+ x6 (- 2.0 2.0)) 2.0)))))) (/ (marctan (/ (+ (* x4 (* 2.0 (+ (- x4) (+ 2.0 (+ (- x6 2.0) (+ x5 2.0)))))) (+ (* 2.0 (* x5 (+ (- x4 2.0) (+ x6 (+ (- 2.0 x5) 2.0))))) (- (- (- (- (* x6 (* 2.0 (+ x4 (+ (- 2.0 x6) (+ 2.0 (- x5 2.0)))))) (* 2.0 (* x6 2.0))) (* x4 (* x6 x5))) (* x4 (^ 2.0 2.0))) (* 2.0 (* x5 2.0))))) (* 4.0 (^ (+ (^ (* x4 (* 2.0 x6)) 0.5) (+ (* (^ x4 0.5) (/ (+ 2.0 (- x6 2.0)) 2.0)) (+ (* (^ 2.0 0.5) (/ (+ x4 (- x6 x5)) 2.0)) (* (^ x6 0.5) (/ (+ x4 (- 2.0 2.0)) 2.0))))) 2.0)))) (+ (^ (* x4 (* 2.0 x6)) 0.5) (+ (* (^ x4 0.5) (/ (+ 2.0 (- x6 2.0)) 2.0)) (+ (* (^ 2.0 0.5) (/ (+ x4 (- x6 x5)) 2.0)) (* (^ x6 0.5) (/ (+ x4 (- 2.0 2.0)) 2.0)))))))) (* 2.0 (/ (* (- (* 3.0 (arccos (/ 1.0 3.0))) 3.14159265) (/ (^ 8.0 0.5) (- (* 4.0 3.14159265) (* 20.0 (- (* 3.0 (arccos (/ 1.0 3.0))) 3.14159265))))) 3.14159265))) (* (+ (* (* (/ (- 1.26 (* (^ x4 0.5) 0.5)) (- 1.26 1.0)) 1.0) (* (/ (^ (* 4.0 x4) 0.5) (+ (- (* (- 2.0) x6) (* x4 2.0)) (+ (* 2.0 x5) (+ (- (* x6 2.0) (* x5 2.0)) (* x4 (+ (- x4) (+ 2.0 (+ (- x6 2.0) (+ x5 2.0))))))))) (marctan (/ (* 4.0 (* x4 (+ (* x4 (* 2.0 (+ (- x4) (+ 2.0 (+ (- x6 2.0) (+ x5 2.0)))))) (+ (* 2.0 (* x5 (+ (- x4 2.0) (+ x6 (+ (- 2.0 x5) 2.0))))) (- (- (- (- (* x6 (* 2.0 (+ x4 (+ (- 2.0 x6) (+ 2.0 (- x5 2.0)))))) (* 2.0 (* x6 2.0))) (* x4 (* x6 x5))) (* x4 (^ 2.0 2.0))) (* 2.0 (* x5 2.0))))))) (^ (+ (- (* (- 2.0) x6) (* x4 2.0)) (+ (* 2.0 x5) (+ (- (* x6 2.0) (* x5 2.0)) (* x4 (+ (- x4) (+ 2.0 (+ (- x6 2.0) (+ x5 2.0)))))))) 2.0))))) (+ (* (* (/ (- 1.26 (* (^ x5 0.5) 0.5)) (- 1.26 1.0)) 1.0) (* (/ (^ (* 4.0 x5) 0.5) (+ (- (* (- 2.0) x4) (* x5 2.0)) (+ (* 2.0 x6) (+ (- (* x4 2.0) (* x6 2.0)) (* x5 (+ (- x5) (+ 2.0 (+ (- x4 2.0) (+ x6 2.0))))))))) (marctan (/ (* 4.0 (* x5 (+ (* x5 (* 2.0 (+ (- x5) (+ 2.0 (+ (- x4 2.0) (+ x6 2.0)))))) (+ (* 2.0 (* x6 (+ (- x5 2.0) (+ x4 (+ (- 2.0 x6) 2.0))))) (- (- (- (- (* x4 (* 2.0 (+ x5 (+ (- 2.0 x4) (+ 2.0 (- x6 2.0)))))) (* 2.0 (* x4 2.0))) (* x5 (* x4 x6))) (* x5 (^ 2.0 2.0))) (* 2.0 (* x6 2.0))))))) (^ (+ (- (* (- 2.0) x4) (* x5 2.0)) (+ (* 2.0 x6) (+ (- (* x4 2.0) (* x6 2.0)) (* x5 (+ (- x5) (+ 2.0 (+ (- x4 2.0) (+ x6 2.0)))))))) 2.0))))) (* (* (/ (- 1.26 (* (^ x6 0.5) 0.5)) (- 1.26 1.0)) 1.0) (* (/ (^ (* 4.0 x6) 0.5) (+ (- (* (- 2.0) x5) (* x6 2.0)) (+ (* 2.0 x4) (+ (- (* x5 2.0) (* x4 2.0)) (* x6 (+ (- x6) (+ 2.0 (+ (- x5 2.0) (+ x4 2.0))))))))) (marctan (/ (* 4.0 (* x6 (+ (* x6 (* 2.0 (+ (- x6) (+ 2.0 (+ (- x5 2.0) (+ x4 2.0)))))) (+ (* 2.0 (* x4 (+ (- x6 2.0) (+ x5 (+ (- 2.0 x4) 2.0))))) (- (- (- (- (* x5 (* 2.0 (+ x6 (+ (- 2.0 x5) (+ 2.0 (- x4 2.0)))))) (* 2.0 (* x5 2.0))) (* x6 (* x5 x4))) (* x6 (^ 2.0 2.0))) (* 2.0 (* x4 2.0))))))) (^ (+ (- (* (- 2.0) x5) (* x6 2.0)) (+ (* 2.0 x4) (+ (- (* x5 2.0) (* x4 2.0)) (* x6 (+ (- x6) (+ 2.0 (+ (- x5 2.0) (+ x4 2.0)))))))) 2.0))))))) (* 8.0 (/ (* (- (* 6.0 (- (* 3.0 (arccos (/ 1.0 3.0))) 3.14159265)) 3.14159265) (/ (^ 2.0 0.5) (* 6.0 (- (* 4.0 3.14159265) (* 20.0 (- (* 3.0 (arccos (/ 1.0 3.0))) 3.14159265)))))) 3.14159265))))) (- 1.0))) 0.0)) :side A))
(assert (! (not (< (+ (* 1.0 2.0) (* (^ (^ (/ (* (^ (^ x4 0.5) 2.0) (* (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0))) (+ (- (- (* (- (^ (^ x4 0.5) 2.0)) (^ (^ x4 0.5) 2.0)) (^ (^ (^ x5 0.5) 2.0) 2.0)) (^ (^ (^ x6 0.5) 2.0) 2.0)) (+ (* 2.0 (* (^ (^ x4 0.5) 2.0) (^ (^ x6 0.5) 2.0))) (+ (* 2.0 (* (^ (^ x4 0.5) 2.0) (^ (^ x5 0.5) 2.0))) (* 2.0 (* (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0))))))) 0.5) 2.0) (- 1.0))) 0.0)) :side B))
(check-sat)
(exit)
