(set-logic QF_NRA)
(declare-fun x1 () Real)
(declare-fun x2 () Real)
(declare-fun x3 () Real)
(declare-fun x4 () Real)
(declare-fun x5 () Real)
(declare-fun x6 () Real)
(assert (<= 6.3504 x1))
(assert (<= x1 7.02674064))
(assert (<= 4.0 x2))
(assert (<= x2 8.0))
(assert (<= 4.0 x3))
(assert (<= x3 6.06887582536))
(assert (<= 4.0 x4))
(assert (<= x4 8.0))
(assert (<= 4.0 x5))
(assert (<= x5 6.06887582536))
(assert (<= 4.0 x6))
(assert (<= x6 8.0))
(assert (! (not (< (+ (* 1.0 (^ 1.34 2.0)) (* (^ (^ (/ (* (^ (^ x1 0.5) 2.0) (* (^ (^ x3 0.5) 2.0) (^ (^ x5 0.5) 2.0))) (+ (- (- (* (- (^ (^ x1 0.5) 2.0)) (^ (^ x1 0.5) 2.0)) (^ (^ (^ x3 0.5) 2.0) 2.0)) (^ (^ (^ x5 0.5) 2.0) 2.0)) (+ (* 2.0 (* (^ (^ x1 0.5) 2.0) (^ (^ x5 0.5) 2.0))) (+ (* 2.0 (* (^ (^ x1 0.5) 2.0) (^ (^ x3 0.5) 2.0))) (* 2.0 (* (^ (^ x3 0.5) 2.0) (^ (^ x5 0.5) 2.0))))))) 0.5) 2.0) (- 1.0))) 0.0)) :side A))
(assert (! (not (< (+ (* 1.0 0.0560305) (+ (* (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* x1 (+ (* x1 (* x4 (+ (- x1) (+ x2 (+ (- x3 x4) (+ x5 x6)))))) (+ (* x2 (* x5 (+ (- x1 x2) (+ x3 (+ (- x4 x5) x6))))) (- (- (- (- (* x3 (* x6 (+ x1 (+ (- x2 x3) (+ x4 (- x5 x6)))))) (* x2 (* x3 x4))) (* x1 (* x3 x5))) (* x1 (* x2 x6))) (* x4 (* x5 x6))))))) 0.5) (- (+ (- (* (- x2) x3) (* x1 x4)) (+ (* x2 x5) (+ (- (* x3 x6) (* x5 x6)) (* x1 (+ (- x1) (+ x2 (+ (- x3 x4) (+ x5 x6))))))))))) (- 0.0445813)) (* (+ (- (/ (^ (+ (* 2.0 (* x1 (+ (- 2.0) (+ 2.0 (+ (- 2.0 x1) (+ x3 x5)))))) (+ (* 2.0 (* x3 (+ (- 2.0 2.0) (+ 2.0 (+ (- x1 x3) x5))))) (- (- (- (- (* 2.0 (* x5 (+ 2.0 (+ (- 2.0 2.0) (+ x1 (- x3 x5)))))) (* 2.0 (* 2.0 x1))) (* 2.0 (* 2.0 x3))) (* 2.0 (* 2.0 x5))) (* x1 (* x3 x5))))) 0.5) 12.0) (- (* (+ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* x3 (+ (* x3 (* 2.0 (+ (- x3) (+ 2.0 (+ (- x1 2.0) (+ x5 2.0)))))) (+ (* 2.0 (* x5 (+ (- x3 2.0) (+ x1 (+ (- 2.0 x5) 2.0))))) (- (- (- (- (* x1 (* 2.0 (+ x3 (+ (- 2.0 x1) (+ 2.0 (- x5 2.0)))))) (* 2.0 (* x1 2.0))) (* x3 (* x1 x5))) (* x3 (^ 2.0 2.0))) (* 2.0 (* x5 2.0))))))) 0.5) (- (+ (- (* (- 2.0) x1) (* x3 2.0)) (+ (* 2.0 x5) (+ (- (* x1 2.0) (* x5 2.0)) (* x3 (+ (- x3) (+ 2.0 (+ (- x1 2.0) (+ x5 2.0))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* 2.0 (+ (* 2.0 (* x5 (+ (- 2.0) (+ x1 (+ (- x3 x5) (+ 2.0 2.0)))))) (+ (* x1 (* 2.0 (+ (- 2.0 x1) (+ x3 (+ (- x5 2.0) 2.0))))) (- (- (- (- (* x3 (* 2.0 (+ 2.0 (+ (- x1 x3) (+ x5 (- 2.0 2.0)))))) (* x1 (* x3 x5))) (* 2.0 (* x3 2.0))) (* 2.0 (* x1 2.0))) (* x5 (^ 2.0 2.0))))))) 0.5) (- (+ (- (* (- x1) x3) (* 2.0 x5)) (+ (* x1 2.0) (+ (- (* x3 2.0) (^ 2.0 2.0)) (* 2.0 (+ (- 2.0) (+ x1 (+ (- x3 x5) (+ 2.0 2.0))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* x1 (+ (* x1 (* 2.0 (+ (- x1) (+ x3 (+ (- 2.0 2.0) (+ 2.0 x5)))))) (+ (* x3 (* 2.0 (+ (- x1 x3) (+ 2.0 (+ (- 2.0 2.0) x5))))) (- (- (- (- (* 2.0 (* x5 (+ x1 (+ (- x3 2.0) (+ 2.0 (- 2.0 x5)))))) (* x3 (^ 2.0 2.0))) (* x1 (^ 2.0 2.0))) (* x1 (* x3 x5))) (* 2.0 (* 2.0 x5))))))) 0.5) (- (+ (- (* (- x3) 2.0) (* x1 2.0)) (+ (* x3 2.0) (+ (- (* 2.0 x5) (* 2.0 x5)) (* x1 (+ (- x1) (+ x3 (+ (- 2.0 2.0) (+ 2.0 x5))))))))))) 3.14159265))) (+ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* x5 (+ (* x5 (* 2.0 (+ (- x5) (+ 2.0 (+ (- x3 2.0) (+ x1 2.0)))))) (+ (* 2.0 (* x1 (+ (- x5 2.0) (+ x3 (+ (- 2.0 x1) 2.0))))) (- (- (- (- (* x3 (* 2.0 (+ x5 (+ (- 2.0 x3) (+ 2.0 (- x1 2.0)))))) (* 2.0 (* x3 2.0))) (* x5 (* x3 x1))) (* x5 (^ 2.0 2.0))) (* 2.0 (* x1 2.0))))))) 0.5) (- (+ (- (* (- 2.0) x3) (* x5 2.0)) (+ (* 2.0 x1) (+ (- (* x3 2.0) (* x1 2.0)) (* x5 (+ (- x5) (+ 2.0 (+ (- x3 2.0) (+ x1 2.0))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* 2.0 (+ (* 2.0 (* x1 (+ (- 2.0) (+ x3 (+ (- x5 x1) (+ 2.0 2.0)))))) (+ (* x3 (* 2.0 (+ (- 2.0 x3) (+ x5 (+ (- x1 2.0) 2.0))))) (- (- (- (- (* x5 (* 2.0 (+ 2.0 (+ (- x3 x5) (+ x1 (- 2.0 2.0)))))) (* x3 (* x5 x1))) (* 2.0 (* x5 2.0))) (* 2.0 (* x3 2.0))) (* x1 (^ 2.0 2.0))))))) 0.5) (- (+ (- (* (- x3) x5) (* 2.0 x1)) (+ (* x3 2.0) (+ (- (* x5 2.0) (^ 2.0 2.0)) (* 2.0 (+ (- 2.0) (+ x3 (+ (- x5 x1) (+ 2.0 2.0))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* x3 (+ (* x3 (* 2.0 (+ (- x3) (+ x5 (+ (- 2.0 2.0) (+ 2.0 x1)))))) (+ (* x5 (* 2.0 (+ (- x3 x5) (+ 2.0 (+ (- 2.0 2.0) x1))))) (- (- (- (- (* 2.0 (* x1 (+ x3 (+ (- x5 2.0) (+ 2.0 (- 2.0 x1)))))) (* x5 (^ 2.0 2.0))) (* x3 (^ 2.0 2.0))) (* x3 (* x5 x1))) (* 2.0 (* 2.0 x1))))))) 0.5) (- (+ (- (* (- x5) 2.0) (* x3 2.0)) (+ (* x5 2.0) (+ (- (* 2.0 x1) (* 2.0 x1)) (* x3 (+ (- x3) (+ x5 (+ (- 2.0 2.0) (+ 2.0 x1))))))))))) 3.14159265))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* x1 (+ (* x1 (* 2.0 (+ (- x1) (+ 2.0 (+ (- x5 2.0) (+ x3 2.0)))))) (+ (* 2.0 (* x3 (+ (- x1 2.0) (+ x5 (+ (- 2.0 x3) 2.0))))) (- (- (- (- (* x5 (* 2.0 (+ x1 (+ (- 2.0 x5) (+ 2.0 (- x3 2.0)))))) (* 2.0 (* x5 2.0))) (* x1 (* x5 x3))) (* x1 (^ 2.0 2.0))) (* 2.0 (* x3 2.0))))))) 0.5) (- (+ (- (* (- 2.0) x5) (* x1 2.0)) (+ (* 2.0 x3) (+ (- (* x5 2.0) (* x3 2.0)) (* x1 (+ (- x1) (+ 2.0 (+ (- x5 2.0) (+ x3 2.0))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* 2.0 (+ (* 2.0 (* x3 (+ (- 2.0) (+ x5 (+ (- x1 x3) (+ 2.0 2.0)))))) (+ (* x5 (* 2.0 (+ (- 2.0 x5) (+ x1 (+ (- x3 2.0) 2.0))))) (- (- (- (- (* x1 (* 2.0 (+ 2.0 (+ (- x5 x1) (+ x3 (- 2.0 2.0)))))) (* x5 (* x1 x3))) (* 2.0 (* x1 2.0))) (* 2.0 (* x5 2.0))) (* x3 (^ 2.0 2.0))))))) 0.5) (- (+ (- (* (- x5) x1) (* 2.0 x3)) (+ (* x5 2.0) (+ (- (* x1 2.0) (^ 2.0 2.0)) (* 2.0 (+ (- 2.0) (+ x5 (+ (- x1 x3) (+ 2.0 2.0))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* x5 (+ (* x5 (* 2.0 (+ (- x5) (+ x1 (+ (- 2.0 2.0) (+ 2.0 x3)))))) (+ (* x1 (* 2.0 (+ (- x5 x1) (+ 2.0 (+ (- 2.0 2.0) x3))))) (- (- (- (- (* 2.0 (* x3 (+ x5 (+ (- x1 2.0) (+ 2.0 (- 2.0 x3)))))) (* x1 (^ 2.0 2.0))) (* x5 (^ 2.0 2.0))) (* x5 (* x1 x3))) (* 2.0 (* 2.0 x3))))))) 0.5) (- (+ (- (* (- x1) 2.0) (* x5 2.0)) (+ (* x1 2.0) (+ (- (* 2.0 x3) (* 2.0 x3)) (* x5 (+ (- x5) (+ x1 (+ (- 2.0 2.0) (+ 2.0 x3))))))))))) 3.14159265))))) (* 2.0 (/ (* (- (* 3.0 (arccos (/ 1.0 3.0))) 3.14159265) (/ (^ 8.0 0.5) (- (* 4.0 3.14159265) (* 20.0 (- (* 3.0 (arccos (/ 1.0 3.0))) 3.14159265))))) 3.14159265))) (* (+ 0.0 (+ (* (/ (- 1.26 (* (^ x3 0.5) 0.5)) (- 1.26 1.0)) (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* x3 (+ (* x3 (* 2.0 (+ (- x3) (+ 2.0 (+ (- x1 2.0) (+ x5 2.0)))))) (+ (* 2.0 (* x5 (+ (- x3 2.0) (+ x1 (+ (- 2.0 x5) 2.0))))) (- (- (- (- (* x1 (* 2.0 (+ x3 (+ (- 2.0 x1) (+ 2.0 (- x5 2.0)))))) (* 2.0 (* x1 2.0))) (* x3 (* x1 x5))) (* x3 (^ 2.0 2.0))) (* 2.0 (* x5 2.0))))))) 0.5) (- (+ (- (* (- 2.0) x1) (* x3 2.0)) (+ (* 2.0 x5) (+ (- (* x1 2.0) (* x5 2.0)) (* x3 (+ (- x3) (+ 2.0 (+ (- x1 2.0) (+ x5 2.0)))))))))))) (* (/ (- 1.26 (* (^ x5 0.5) 0.5)) (- 1.26 1.0)) (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* x5 (+ (* x5 (* 2.0 (+ (- x5) (+ 2.0 (+ (- x3 2.0) (+ x1 2.0)))))) (+ (* 2.0 (* x1 (+ (- x5 2.0) (+ x3 (+ (- 2.0 x1) 2.0))))) (- (- (- (- (* x3 (* 2.0 (+ x5 (+ (- 2.0 x3) (+ 2.0 (- x1 2.0)))))) (* 2.0 (* x3 2.0))) (* x5 (* x3 x1))) (* x5 (^ 2.0 2.0))) (* 2.0 (* x1 2.0))))))) 0.5) (- (+ (- (* (- 2.0) x3) (* x5 2.0)) (+ (* 2.0 x1) (+ (- (* x3 2.0) (* x1 2.0)) (* x5 (+ (- x5) (+ 2.0 (+ (- x3 2.0) (+ x1 2.0)))))))))))))) (* 8.0 (/ (* (- (* 6.0 (- (* 3.0 (arccos (/ 1.0 3.0))) 3.14159265)) 3.14159265) (/ (^ 2.0 0.5) (* 6.0 (- (* 4.0 3.14159265) (* 20.0 (- (* 3.0 (arccos (/ 1.0 3.0))) 3.14159265)))))) 3.14159265))))) (* (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* x1 (+ (* x1 (* x4 (+ (- x1) (+ x2 (+ (- x3 x4) (+ x5 x6)))))) (+ (* x2 (* x5 (+ (- x1 x2) (+ x3 (+ (- x4 x5) x6))))) (- (- (- (- (* x3 (* x6 (+ x1 (+ (- x2 x3) (+ x4 (- x5 x6)))))) (* x2 (* x3 x4))) (* x1 (* x3 x5))) (* x1 (* x2 x6))) (* x4 (* x5 x6))))))) 0.5) (- (+ (- (* (- x2) x3) (* x1 x4)) (+ (* x2 x5) (+ (- (* x3 x6) (* x5 x6)) (* x1 (+ (- x1) (+ x2 (+ (- x3 x4) (+ x5 x6))))))))))) (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* x1 (+ (* x1 (* 2.0 (+ (- x1) (+ 2.0 (+ (- x3 2.0) (+ x5 2.0)))))) (+ (* 2.0 (* x5 (+ (- x1 2.0) (+ x3 (+ (- 2.0 x5) 2.0))))) (- (- (- (- (* x3 (* 2.0 (+ x1 (+ (- 2.0 x3) (+ 2.0 (- x5 2.0)))))) (* 2.0 (* x3 2.0))) (* x1 (* x3 x5))) (* x1 (^ 2.0 2.0))) (* 2.0 (* x5 2.0))))))) 0.5) (- (+ (- (* (- 2.0) x3) (* x1 2.0)) (+ (* 2.0 x5) (+ (- (* x3 2.0) (* x5 2.0)) (* x1 (+ (- x1) (+ 2.0 (+ (- x3 2.0) (+ x5 2.0)))))))))))) 0.008)) (- 1.0)))) 0.0)) :side B))
(assert (! (not (< (+ (/ (+ (- (- (* (- x1) (* x1 (^ x4 2.0))) (* x2 (* x2 (^ x5 2.0)))) (* x3 (* x3 (^ x6 2.0)))) (+ (* 2.0 (* x1 (* x2 (* x4 x5)))) (+ (* 2.0 (* x1 (* x3 (* x4 x6)))) (* 2.0 (* x2 (* x3 (* x5 x6))))))) (* (+ (* x1 (* x4 (+ (- x1) (+ x2 (+ (- x3 x4) (+ x5 x6)))))) (+ (* x2 (* x5 (+ (- x1 x2) (+ x3 (+ (- x4 x5) x6))))) (- (- (- (- (* x3 (* x6 (+ x1 (+ (- x2 x3) (+ x4 (- x5 x6)))))) (* x2 (* x3 x4))) (* x1 (* x3 x5))) (* x1 (* x2 x6))) (* x4 (* x5 x6))))) 4.0)) (* 1.0 (- 2.0))) 0.0)) :side B))
(assert (! (not (< (+ (* 1.0 1.074) (* (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* x1 (+ (* x1 (* x4 (+ (- x1) (+ x2 (+ (- x3 x4) (+ x5 x6)))))) (+ (* x2 (* x5 (+ (- x1 x2) (+ x3 (+ (- x4 x5) x6))))) (- (- (- (- (* x3 (* x6 (+ x1 (+ (- x2 x3) (+ x4 (- x5 x6)))))) (* x2 (* x3 x4))) (* x1 (* x3 x5))) (* x1 (* x2 x6))) (* x4 (* x5 x6))))))) 0.5) (- (+ (- (* (- x2) x3) (* x1 x4)) (+ (* x2 x5) (+ (- (* x3 x6) (* x5 x6)) (* x1 (+ (- x1) (+ x2 (+ (- x3 x4) (+ x5 x6))))))))))) (- 1.0))) 0.0)) :side B))
(assert (! (not (< (+ (* 1.0 2.0) (* (^ (^ (/ (* (^ (^ x1 0.5) 2.0) (* (^ (^ x2 0.5) 2.0) (^ (^ x6 0.5) 2.0))) (+ (- (- (* (- (^ (^ x1 0.5) 2.0)) (^ (^ x1 0.5) 2.0)) (^ (^ (^ x2 0.5) 2.0) 2.0)) (^ (^ (^ x6 0.5) 2.0) 2.0)) (+ (* 2.0 (* (^ (^ x1 0.5) 2.0) (^ (^ x6 0.5) 2.0))) (+ (* 2.0 (* (^ (^ x1 0.5) 2.0) (^ (^ x2 0.5) 2.0))) (* 2.0 (* (^ (^ x2 0.5) 2.0) (^ (^ x6 0.5) 2.0))))))) 0.5) 2.0) (- 1.0))) 0.0)) :side B))
(assert (! (not (< (+ (^ (^ (/ (* (^ (^ x1 0.5) 2.0) (* (^ (^ x2 0.5) 2.0) (^ (^ x6 0.5) 2.0))) (+ (- (- (* (- (^ (^ x1 0.5) 2.0)) (^ (^ x1 0.5) 2.0)) (^ (^ (^ x2 0.5) 2.0) 2.0)) (^ (^ (^ x6 0.5) 2.0) 2.0)) (+ (* 2.0 (* (^ (^ x1 0.5) 2.0) (^ (^ x6 0.5) 2.0))) (+ (* 2.0 (* (^ (^ x1 0.5) 2.0) (^ (^ x2 0.5) 2.0))) (* 2.0 (* (^ (^ x2 0.5) 2.0) (^ (^ x6 0.5) 2.0))))))) 0.5) 2.0) (* 1.0 (* 1.34 (- 1.34)))) 0.0)) :side B))
(check-sat)
(exit)
