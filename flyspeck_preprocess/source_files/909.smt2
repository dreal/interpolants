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
(assert (<= 4.0 x4))
(assert (<= x4 4.0))
(assert (<= 8.0 x5))
(assert (<= x5 8.0))
(assert (<= 8.0 x6))
(assert (<= x6 8.0))
(assert (not (< (+ (* 1.0 0.2759) (+ (* 1.0 (* 2.0 0.103)) (+ (* 1.0 (* 2.0 (- 0.11))) (+ (* (* (+ 1.0 (- (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) (* (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) (+ 1.0 (* (- (^ x1 0.5) 2.0) (/ (- 0.0 1.0) (- 2.52 2.0))))))) (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ (^ x1 0.5) 2.0) (+ (* (^ (^ x1 0.5) 2.0) (* (^ (^ x4 0.5) 2.0) (+ (- (^ (^ x1 0.5) 2.0)) (+ (^ (^ x2 0.5) 2.0) (+ (- (^ (^ x3 0.5) 2.0) (^ (^ x4 0.5) 2.0)) (+ (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0))))))) (+ (* (^ (^ x2 0.5) 2.0) (* (^ (^ x5 0.5) 2.0) (+ (- (^ (^ x1 0.5) 2.0) (^ (^ x2 0.5) 2.0)) (+ (^ (^ x3 0.5) 2.0) (+ (- (^ (^ x4 0.5) 2.0) (^ (^ x5 0.5) 2.0)) (^ (^ x6 0.5) 2.0)))))) (- (- (- (- (* (^ (^ x3 0.5) 2.0) (* (^ (^ x6 0.5) 2.0) (+ (^ (^ x1 0.5) 2.0) (+ (- (^ (^ x2 0.5) 2.0) (^ (^ x3 0.5) 2.0)) (+ (^ (^ x4 0.5) 2.0) (- (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0))))))) (* (^ (^ x2 0.5) 2.0) (* (^ (^ x3 0.5) 2.0) (^ (^ x4 0.5) 2.0)))) (* (^ (^ x1 0.5) 2.0) (* (^ (^ x3 0.5) 2.0) (^ (^ x5 0.5) 2.0)))) (* (^ (^ x1 0.5) 2.0) (* (^ (^ x2 0.5) 2.0) (^ (^ x6 0.5) 2.0)))) (* (^ (^ x4 0.5) 2.0) (* (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0)))))))) 0.5) (- (+ (- (* (- (^ (^ x2 0.5) 2.0)) (^ (^ x3 0.5) 2.0)) (* (^ (^ x1 0.5) 2.0) (^ (^ x4 0.5) 2.0))) (+ (* (^ (^ x2 0.5) 2.0) (^ (^ x5 0.5) 2.0)) (+ (- (* (^ (^ x3 0.5) 2.0) (^ (^ x6 0.5) 2.0)) (* (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0))) (* (^ (^ x1 0.5) 2.0) (+ (- (^ (^ x1 0.5) 2.0)) (+ (^ (^ x2 0.5) 2.0) (+ (- (^ (^ x3 0.5) 2.0) (^ (^ x4 0.5) 2.0)) (+ (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0))))))))))))) (- 1.0)) (+ (* (* (+ 1.0 (- (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) (* (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) (+ 1.0 (* (- (^ x2 0.5) 2.0) (/ (- 0.0 1.0) (- 2.52 2.0))))))) (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ (^ x2 0.5) 2.0) (+ (* (^ (^ x2 0.5) 2.0) (* (^ (^ x5 0.5) 2.0) (+ (- (^ (^ x2 0.5) 2.0)) (+ (^ (^ x3 0.5) 2.0) (+ (- (^ (^ x1 0.5) 2.0) (^ (^ x5 0.5) 2.0)) (+ (^ (^ x6 0.5) 2.0) (^ (^ x4 0.5) 2.0))))))) (+ (* (^ (^ x3 0.5) 2.0) (* (^ (^ x6 0.5) 2.0) (+ (- (^ (^ x2 0.5) 2.0) (^ (^ x3 0.5) 2.0)) (+ (^ (^ x1 0.5) 2.0) (+ (- (^ (^ x5 0.5) 2.0) (^ (^ x6 0.5) 2.0)) (^ (^ x4 0.5) 2.0)))))) (- (- (- (- (* (^ (^ x1 0.5) 2.0) (* (^ (^ x4 0.5) 2.0) (+ (^ (^ x2 0.5) 2.0) (+ (- (^ (^ x3 0.5) 2.0) (^ (^ x1 0.5) 2.0)) (+ (^ (^ x5 0.5) 2.0) (- (^ (^ x6 0.5) 2.0) (^ (^ x4 0.5) 2.0))))))) (* (^ (^ x3 0.5) 2.0) (* (^ (^ x1 0.5) 2.0) (^ (^ x5 0.5) 2.0)))) (* (^ (^ x2 0.5) 2.0) (* (^ (^ x1 0.5) 2.0) (^ (^ x6 0.5) 2.0)))) (* (^ (^ x2 0.5) 2.0) (* (^ (^ x3 0.5) 2.0) (^ (^ x4 0.5) 2.0)))) (* (^ (^ x5 0.5) 2.0) (* (^ (^ x6 0.5) 2.0) (^ (^ x4 0.5) 2.0)))))))) 0.5) (- (+ (- (* (- (^ (^ x3 0.5) 2.0)) (^ (^ x1 0.5) 2.0)) (* (^ (^ x2 0.5) 2.0) (^ (^ x5 0.5) 2.0))) (+ (* (^ (^ x3 0.5) 2.0) (^ (^ x6 0.5) 2.0)) (+ (- (* (^ (^ x1 0.5) 2.0) (^ (^ x4 0.5) 2.0)) (* (^ (^ x6 0.5) 2.0) (^ (^ x4 0.5) 2.0))) (* (^ (^ x2 0.5) 2.0) (+ (- (^ (^ x2 0.5) 2.0)) (+ (^ (^ x3 0.5) 2.0) (+ (- (^ (^ x1 0.5) 2.0) (^ (^ x5 0.5) 2.0)) (+ (^ (^ x6 0.5) 2.0) (^ (^ x4 0.5) 2.0))))))))))))) (- 1.0)) (+ (* (* (+ 1.0 (- (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) (* (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) (+ 1.0 (* (- (^ x3 0.5) 2.0) (/ (- 0.0 1.0) (- 2.52 2.0))))))) (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ (^ x3 0.5) 2.0) (+ (* (^ (^ x3 0.5) 2.0) (* (^ (^ x6 0.5) 2.0) (+ (- (^ (^ x3 0.5) 2.0)) (+ (^ (^ x1 0.5) 2.0) (+ (- (^ (^ x2 0.5) 2.0) (^ (^ x6 0.5) 2.0)) (+ (^ (^ x4 0.5) 2.0) (^ (^ x5 0.5) 2.0))))))) (+ (* (^ (^ x1 0.5) 2.0) (* (^ (^ x4 0.5) 2.0) (+ (- (^ (^ x3 0.5) 2.0) (^ (^ x1 0.5) 2.0)) (+ (^ (^ x2 0.5) 2.0) (+ (- (^ (^ x6 0.5) 2.0) (^ (^ x4 0.5) 2.0)) (^ (^ x5 0.5) 2.0)))))) (- (- (- (- (* (^ (^ x2 0.5) 2.0) (* (^ (^ x5 0.5) 2.0) (+ (^ (^ x3 0.5) 2.0) (+ (- (^ (^ x1 0.5) 2.0) (^ (^ x2 0.5) 2.0)) (+ (^ (^ x6 0.5) 2.0) (- (^ (^ x4 0.5) 2.0) (^ (^ x5 0.5) 2.0))))))) (* (^ (^ x1 0.5) 2.0) (* (^ (^ x2 0.5) 2.0) (^ (^ x6 0.5) 2.0)))) (* (^ (^ x3 0.5) 2.0) (* (^ (^ x2 0.5) 2.0) (^ (^ x4 0.5) 2.0)))) (* (^ (^ x3 0.5) 2.0) (* (^ (^ x1 0.5) 2.0) (^ (^ x5 0.5) 2.0)))) (* (^ (^ x6 0.5) 2.0) (* (^ (^ x4 0.5) 2.0) (^ (^ x5 0.5) 2.0)))))))) 0.5) (- (+ (- (* (- (^ (^ x1 0.5) 2.0)) (^ (^ x2 0.5) 2.0)) (* (^ (^ x3 0.5) 2.0) (^ (^ x6 0.5) 2.0))) (+ (* (^ (^ x1 0.5) 2.0) (^ (^ x4 0.5) 2.0)) (+ (- (* (^ (^ x2 0.5) 2.0) (^ (^ x5 0.5) 2.0)) (* (^ (^ x4 0.5) 2.0) (^ (^ x5 0.5) 2.0))) (* (^ (^ x3 0.5) 2.0) (+ (- (^ (^ x3 0.5) 2.0)) (+ (^ (^ x1 0.5) 2.0) (+ (- (^ (^ x2 0.5) 2.0) (^ (^ x6 0.5) 2.0)) (+ (^ (^ x4 0.5) 2.0) (^ (^ x5 0.5) 2.0))))))))))))) (- 1.0)) (+ (* 1.0 3.14159265) (* 1.0 (* (/ (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (+ (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) (- (+ (/ 3.14159265 2.0) (arctan2 (^ (* 4.0 (* (^ 2.0 2.0) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0))))))) (+ (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (^ 2.0 2.0)))))) (- (- (- (- (* (^ 2.0 2.0) (* (^ 2.0 2.0) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (- (^ 2.0 2.0) (^ 2.0 2.0))))))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))) (* (^ 2.0 2.0) (^ (^ 2.0 2.0) 2.0))))))) 0.5) (- (+ (- (* (- (^ 2.0 2.0)) (^ 2.0 2.0)) (^ (^ 2.0 2.0) 2.0)) (+ (^ (^ 2.0 2.0) 2.0) (+ (- (^ (^ 2.0 2.0) 2.0) (^ (^ 2.0 2.0) 2.0)) (* (^ 2.0 2.0) (+ (- (^ 2.0 2.0)) (+ (^ 2.0 2.0) (+ (- (^ 2.0 2.0) (^ 2.0 2.0)) (+ (^ 2.0 2.0) (^ 2.0 2.0)))))))))))) 3.14159265))) 3.14159265) 3.14159265))))))))) 0.0)))
(check-sat)
(exit)
