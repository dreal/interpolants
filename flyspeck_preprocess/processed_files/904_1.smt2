(set-logic QF_NRA)
(declare-fun x1 () Real)
(declare-fun x2 () Real)
(declare-fun x3 () Real)
(declare-fun x4 () Real)
(declare-fun x5 () Real)
(declare-fun x6 () Real)
(assert (<= 4.0 x1))
(assert (<= x1 6.3504))
(assert (<= 4.0 x2))
(assert (<= x2 6.3504))
(assert (<= 4.0 x3))
(assert (<= x3 6.3504))
(assert (<= 9.0601 x4))
(assert (<= x4 9.0601))
(assert (<= 9.0601 x5))
(assert (<= x5 9.0601))
(assert (<= 4.0 x6))
(assert (<= x6 6.3504))
(assert (! (not (< (+ (* x1 (* x4 (+ (- x1) (+ x2 (+ (- x3 x4) (+ x5 x6)))))) (+ (* x2 (* x5 (+ (- x1 x2) (+ x3 (+ (- x4 x5) x6))))) (- (- (- (- (* x3 (* x6 (+ x1 (+ (- x2 x3) (+ x4 (- x5 x6)))))) (* x2 (* x3 x4))) (* x1 (* x3 x5))) (* x1 (* x2 x6))) (* x4 (* x5 x6))))) 0.0)) :side A))
(assert (! (not (< (+ (* 1.0 0.6548) (+ (* 1.0 (* 2.0 (- 0.11))) (+ (* (* (+ 1.0 (- (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) (* (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) (+ 1.0 (* (- (^ x1 0.5) 2.0) (/ (- 0.0 1.0) (- 2.52 2.0))))))) (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ (^ x1 0.5) 2.0) (+ (* (^ (^ x1 0.5) 2.0) (* (^ (^ x4 0.5) 2.0) (+ (- (^ (^ x1 0.5) 2.0)) (+ (^ (^ x2 0.5) 2.0) (+ (- (^ (^ x3 0.5) 2.0) (^ (^ x4 0.5) 2.0)) (+ (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0))))))) (+ (* (^ (^ x2 0.5) 2.0) (* (^ (^ x5 0.5) 2.0) (+ (- (^ (^ x1 0.5) 2.0) (^ (^ x2 0.5) 2.0)) (+ (^ (^ x3 0.5) 2.0) (+ (- (^ (^ x4 0.5) 2.0) (^ (^ x5 0.5) 2.0)) (^ (^ x6 0.5) 2.0)))))) (- (- (- (- (* (^ (^ x3 0.5) 2.0) (* (^ (^ x6 0.5) 2.0) (+ (^ (^ x1 0.5) 2.0) (+ (- (^ (^ x2 0.5) 2.0) (^ (^ x3 0.5) 2.0)) (+ (^ (^ x4 0.5) 2.0) (- (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0))))))) (* (^ (^ x2 0.5) 2.0) (* (^ (^ x3 0.5) 2.0) (^ (^ x4 0.5) 2.0)))) (* (^ (^ x1 0.5) 2.0) (* (^ (^ x3 0.5) 2.0) (^ (^ x5 0.5) 2.0)))) (* (^ (^ x1 0.5) 2.0) (* (^ (^ x2 0.5) 2.0) (^ (^ x6 0.5) 2.0)))) (* (^ (^ x4 0.5) 2.0) (* (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0)))))))) 0.5) (- (+ (- (* (- (^ (^ x2 0.5) 2.0)) (^ (^ x3 0.5) 2.0)) (* (^ (^ x1 0.5) 2.0) (^ (^ x4 0.5) 2.0))) (+ (* (^ (^ x2 0.5) 2.0) (^ (^ x5 0.5) 2.0)) (+ (- (* (^ (^ x3 0.5) 2.0) (^ (^ x6 0.5) 2.0)) (* (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0))) (* (^ (^ x1 0.5) 2.0) (+ (- (^ (^ x1 0.5) 2.0)) (+ (^ (^ x2 0.5) 2.0) (+ (- (^ (^ x3 0.5) 2.0) (^ (^ x4 0.5) 2.0)) (+ (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0))))))))))))) (- 1.0)) (+ (* (* (+ 1.0 (- (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) (* (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) (+ 1.0 (* (- (^ x2 0.5) 2.0) (/ (- 0.0 1.0) (- 2.52 2.0))))))) (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ (^ x2 0.5) 2.0) (+ (* (^ (^ x2 0.5) 2.0) (* (^ (^ x5 0.5) 2.0) (+ (- (^ (^ x2 0.5) 2.0)) (+ (^ (^ x3 0.5) 2.0) (+ (- (^ (^ x1 0.5) 2.0) (^ (^ x5 0.5) 2.0)) (+ (^ (^ x6 0.5) 2.0) (^ (^ x4 0.5) 2.0))))))) (+ (* (^ (^ x3 0.5) 2.0) (* (^ (^ x6 0.5) 2.0) (+ (- (^ (^ x2 0.5) 2.0) (^ (^ x3 0.5) 2.0)) (+ (^ (^ x1 0.5) 2.0) (+ (- (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0)) (^ (^ x4 0.5) 2.0)))))) (- (- (- (- (* (^ (^ x1 0.5) 2.0) (* (^ (^ x4 0.5) 2.0) (+ (^ (^ x2 0.5) 2.0) (+ (- (^ (^ x3 0.5) 2.0) (^ (^ x1 0.5) 2.0)) (+ (^ (^ x5 0.5) 2.0) (- (^ (^ x6 0.5) 2.0) (^ (^ x4 0.5) 2.0))))))) (* (^ (^ x3 0.5) 2.0) (* (^ (^ x1 0.5) 2.0) (^ (^ x5 0.5) 2.0)))) (* (^ (^ x2 0.5) 2.0) (* (^ (^ x1 0.5) 2.0) (^ (^ x6 0.5) 2.0)))) (* (^ (^ x2 0.5) 2.0) (* (^ (^ x3 0.5) 2.0) (^ (^ x4 0.5) 2.0)))) (* (^ (^ x5 0.5) 2.0) (* (^ (^ x6 0.5) 2.0) (^ (^ x4 0.5) 2.0)))))))) 0.5) (- (+ (- (* (- (^ (^ x3 0.5) 2.0)) (^ (^ x1 0.5) 2.0)) (* (^ (^ x2 0.5) 2.0) (^ (^ x5 0.5) 2.0))) (+ (* (^ (^ x3 0.5) 2.0) (^ (^ x6 0.5) 2.0)) (+ (- (* (^ (^ x1 0.5) 2.0) (^ (^ x4 0.5) 2.0)) (* (^ (^ x6 0.5) 2.0) (^ (^ x4 0.5) 2.0))) (* (^ (^ x2 0.5) 2.0) (+ (- (^ (^ x2 0.5) 2.0)) (+ (^ (^ x3 0.5) 2.0) (+ (- (^ (^ x1 0.5) 2.0) (^ (^ x5 0.5) 2.0)) (+ (^ (^ x6 0.5) 2.0) (^ (^ x4 0.5) 2.0))))))))))))) (- 1.0)) (+ (* (* (+ 1.0 (- (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) (* (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) (+ 1.0 (* (- (^ x3 0.5) 2.0) (/ (- 0.0 1.0) (- 2.52 2.0))))))) (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ (^ x3 0.5) 2.0) (+ (* (^ (^ x3 0.5) 2.0) (* (^ (^ x6 0.5) 2.0) (+ (- (^ (^ x3 0.5) 2.0)) (+ (^ (^ x1 0.5) 2.0) (+ (- (^ (^ x2 0.5) 2.0) (^ (^ x6 0.5) 2.0)) (+ (^ (^ x4 0.5) 2.0) (^ (^ x5 0.5) 2.0))))))) (+ (* (^ (^ x1 0.5) 2.0) (* (^ (^ x4 0.5) 2.0) (+ (- (^ (^ x3 0.5) 2.0) (^ (^ x1 0.5) 2.0)) (+ (^ (^ x2 0.5) 2.0) (+ (- (^ (^ x6 0.5) 2.0) (^ (^ x4 0.5) 2.0)) (^ (^ x5 0.5) 2.0)))))) (- (- (- (- (* (^ (^ x2 0.5) 2.0) (* (^ (^ x5 0.5) 2.0) (+ (^ (^ x3 0.5) 2.0) (+ (- (^ (^ x1 0.5) 2.0) (^ (^ x2 0.5) 2.0)) (+ (^ (^ x6 0.5) 2.0) (- (^ (^ x4 0.5) 2.0) (^ (^ x5 0.5) 2.0))))))) (* (^ (^ x1 0.5) 2.0) (* (^ (^ x2 0.5) 2.0) (^ (^ x6 0.5) 2.0)))) (* (^ (^ x3 0.5) 2.0) (* (^ (^ x2 0.5) 2.0) (^ (^ x4 0.5) 2.0)))) (* (^ (^ x3 0.5) 2.0) (* (^ (^ x1 0.5) 2.0) (^ (^ x5 0.5) 2.0)))) (* (^ (^ x6 0.5) 2.0) (* (^ (^ x4 0.5) 2.0) (^ (^ x5 0.5) 2.0)))))))) 0.5) (- (+ (- (* (- (^ (^ x1 0.5) 2.0)) (^ (^ x2 0.5) 2.0)) (* (^ (^ x3 0.5) 2.0) (^ (^ x6 0.5) 2.0))) (+ (* (^ (^ x1 0.5) 2.0) (^ (^ x4 0.5) 2.0)) (+ (- (* (^ (^ x2 0.5) 2.0) (^ (^ x5 0.5) 2.0)) (* (^ (^ x4 0.5) 2.0) (^ (^ x5 0.5) 2.0))) (* (^ (^ x3 0.5) 2.0) (+ (- (^ (^ x3 0.5) 2.0)) (+ (^ (^ x1 0.5) 2.0) (+ (- (^ (^ x2 0.5) 2.0) (^ (^ x6 0.5) 2.0)) (+ (^ (^ x4 0.5) 2.0) (^ (^ x5 0.5) 2.0))))))))))))) (- 1.0)) (+ (* 1.0 3.14159265) (* 1.0 (* (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) 3.14159265)))))))) 0.0)) :side B))
(check-sat)
(exit)
