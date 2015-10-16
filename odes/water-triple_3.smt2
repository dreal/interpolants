(set-logic QF_NRA_ODE)
(declare-fun tau () Real)
(declare-fun x1 () Real)
(declare-fun x2 () Real)
(declare-fun x3 () Real)
(define-ode flow_1 ((= d/dt[x1] (/ (- 5 (* (* 0.5 (^ (* 2 9.80665) 0.5)) (^ x1 0.5))) 2)) 
                    (= d/dt[x2] (/ (+ 3 (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x1 0.5) (^ x2 0.5)))) 4)) 
                    (= d/dt[x3] (/ (+ 4 (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x2 0.5) (^ x3 0.5)))) 3))
		    (= d/dt[tau] 1)))
(define-ode flow_2 ((= d/dt[x1] (/ (- 5 (* (* 0.5 (^ (* 2 9.80665) 0.5)) (^ x1 0.5))) 2)) 
                    (= d/dt[x2] (/ (+ 3 (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x1 0.5) (^ x2 0.5)))) 4)) 
                    (= d/dt[x3] (/ (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x2 0.5) (^ x3 0.5))) 3))
		    (= d/dt[tau] 1)))
(define-ode flow_3 ((= d/dt[x1] (/ (- 5 (* (* 0.5 (^ (* 2 9.80665) 0.5)) (^ x1 0.5))) 2)) 
                    (= d/dt[x2] (/ (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x1 0.5) (^ x2 0.5))) 4)) 
                    (= d/dt[x3] (/ (+ 4 (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x2 0.5) (^ x3 0.5)))) 3))
		    (= d/dt[tau] 1)))
(define-ode flow_4 ((= d/dt[x1] (/ (- 5 (* (* 0.5 (^ (* 2 9.80665) 0.5)) (^ x1 0.5))) 2)) 
                    (= d/dt[x2] (/ (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x1 0.5) (^ x2 0.5))) 4)) 
                    (= d/dt[x3] (/ (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x2 0.5) (^ x3 0.5))) 3))
		    (= d/dt[tau] 1)))
(define-ode flow_5 ((= d/dt[x1] (/ (* (* -0.5 (^ (* 2 9.80665) 0.5)) (^ x1 0.5)) 2)) 
                    (= d/dt[x2] (/ (+ 3 (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x1 0.5) (^ x2 0.5)))) 4)) 
                    (= d/dt[x3] (/ (+ 4 (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x2 0.5) (^ x3 0.5)))) 3))
		    (= d/dt[tau] 1)))
(define-ode flow_6 ((= d/dt[x1] (/ (* (* -0.5 (^ (* 2 9.80665) 0.5)) (^ x1 0.5)) 2)) 
                    (= d/dt[x2] (/ (+ 3 (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x1 0.5) (^ x2 0.5)))) 4)) 
                    (= d/dt[x3] (/ (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x2 0.5) (^ x3 0.5))) 3))
		    (= d/dt[tau] 1)))
(define-ode flow_7 ((= d/dt[x1] (/ (* (* -0.5 (^ (* 2 9.80665) 0.5)) (^ x1 0.5)) 2)) 
                    (= d/dt[x2] (/ (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x1 0.5) (^ x2 0.5))) 4)) 
                    (= d/dt[x3] (/ (+ 4 (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x2 0.5) (^ x3 0.5)))) 3))
		    (= d/dt[tau] 1)))
(define-ode flow_8 ((= d/dt[x1] (/ (* (* -0.5 (^ (* 2 9.80665) 0.5)) (^ x1 0.5)) 2)) 
                    (= d/dt[x2] (/ (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x1 0.5) (^ x2 0.5))) 4)) 
                    (= d/dt[x3] (/ (* (* 0.5 (^ (* 2 9.80665) 0.5)) (- (^ x2 0.5) (^ x3 0.5))) 3))
		    (= d/dt[tau] 1)))
(declare-fun time_0 () Real)  
(declare-fun tau_0_0 () Real) 
(declare-fun tau_0_t () Real) 
(declare-fun mode1_0 () Bool) 
(declare-fun x1_0_0 () Real)  
(declare-fun x1_0_t () Real)  
(declare-fun mode2_0 () Bool)
(declare-fun x2_0_0 () Real)  
(declare-fun x2_0_t () Real)  
(declare-fun mode3_0 () Bool)
(declare-fun x3_0_0 () Real)  
(declare-fun x3_0_t () Real)
(declare-fun time_1 () Real)  
(declare-fun tau_1_0 () Real) 
(declare-fun tau_1_t () Real) 
(declare-fun mode1_1 () Bool) 
(declare-fun x1_1_0 () Real)  
(declare-fun x1_1_t () Real)  
(declare-fun mode2_1 () Bool)
(declare-fun x2_1_0 () Real)  
(declare-fun x2_1_t () Real)  
(declare-fun mode3_1 () Bool)
(declare-fun x3_1_0 () Real)  
(declare-fun x3_1_t () Real)
(declare-fun time_2 () Real)  
(declare-fun tau_2_0 () Real) 
(declare-fun tau_2_t () Real) 
(declare-fun mode1_2 () Bool) 
(declare-fun x1_2_0 () Real)  
(declare-fun x1_2_t () Real)  
(declare-fun mode2_2 () Bool)
(declare-fun x2_2_0 () Real)  
(declare-fun x2_2_t () Real)  
(declare-fun mode3_2 () Bool)
(declare-fun x3_2_0 () Real)  
(declare-fun x3_2_t () Real)
(declare-fun time_3 () Real)  
(declare-fun tau_3_0 () Real) 
(declare-fun tau_3_t () Real) 
(declare-fun mode1_3 () Bool) 
(declare-fun x1_3_0 () Real)  
(declare-fun x1_3_t () Real)  
(declare-fun mode2_3 () Bool)
(declare-fun x2_3_0 () Real)  
(declare-fun x2_3_t () Real)  
(declare-fun mode3_3 () Bool)
(declare-fun x3_3_0 () Real)  
(declare-fun x3_3_t () Real)
(assert (<= 0 time_0))  (assert (<= time_0 1))
(assert (<= 0 tau_0_0)) (assert (<= tau_0_0 1))
(assert (<= 0 tau_0_t)) (assert (<= tau_0_t 1))
(assert (<= 0 x1_0_0)) (assert (<= x1_0_0 10))
(assert (<= 0 x1_0_t)) (assert (<= x1_0_t 10))
(assert (<= 0 x2_0_0)) (assert (<= x2_0_0 10))
(assert (<= 0 x2_0_t)) (assert (<= x2_0_t 10))
(assert (<= 0 x3_0_0)) (assert (<= x3_0_0 10))
(assert (<= 0 x3_0_t)) (assert (<= x3_0_t 10))
(assert (<= 0 time_1))  (assert (<= time_1 1))
(assert (<= 0 tau_1_0)) (assert (<= tau_1_0 1))
(assert (<= 0 tau_1_t)) (assert (<= tau_1_t 1))
(assert (<= 0 x1_1_0)) (assert (<= x1_1_0 10))
(assert (<= 0 x1_1_t)) (assert (<= x1_1_t 10))
(assert (<= 0 x2_1_0)) (assert (<= x2_1_0 10))
(assert (<= 0 x2_1_t)) (assert (<= x2_1_t 10))
(assert (<= 0 x3_1_0)) (assert (<= x3_1_0 10))
(assert (<= 0 x3_1_t)) (assert (<= x3_1_t 10))
(assert (<= 0 time_2))  (assert (<= time_2 1))
(assert (<= 0 tau_2_0)) (assert (<= tau_2_0 1))
(assert (<= 0 tau_2_t)) (assert (<= tau_2_t 1))
(assert (<= 0 x1_2_0)) (assert (<= x1_2_0 10))
(assert (<= 0 x1_2_t)) (assert (<= x1_2_t 10))
(assert (<= 0 x2_2_0)) (assert (<= x2_2_0 10))
(assert (<= 0 x2_2_t)) (assert (<= x2_2_t 10))
(assert (<= 0 x3_2_0)) (assert (<= x3_2_0 10))
(assert (<= 0 x3_2_t)) (assert (<= x3_2_t 10))
(assert (<= 0 time_3))  (assert (<= time_3 1))
(assert (<= 0 tau_3_0)) (assert (<= tau_3_0 1))
(assert (<= 0 tau_3_t)) (assert (<= tau_3_t 1))
(assert (<= 0 x1_3_0)) (assert (<= x1_3_0 10))
(assert (<= 0 x1_3_t)) (assert (<= x1_3_t 10))
(assert (<= 0 x2_3_0)) (assert (<= x2_3_0 10))
(assert (<= 0 x2_3_t)) (assert (<= x2_3_t 10))
(assert (<= 0 x3_3_0)) (assert (<= x3_3_0 10))
(assert (<= 0 x3_3_t)) (assert (<= x3_3_t 10))
(assert (= tau_0_0 0))
(assert (= mode1_0 true))
(assert (and (>= x1_0_0 (- 5 0.1)) (<= x1_0_0 (+ 5 0.1))))
(assert (= mode2_0 true))
(assert (and (>= x2_0_0 (- 5 0.1)) (<= x2_0_0 (+ 5 0.1))))
(assert (= mode3_0 true))
(assert (and (>= x3_0_0 (- 5 0.1)) (<= x3_0_0 (+ 5 0.1))))
(assert (and (>= tau_0_0 0) (<= tau_0_0 1)
             (>= tau_0_t 0) (<= tau_0_t 1)
             (forall_t 1 [0 time_0] (>= tau_0_t 0))
             (forall_t 2 [0 time_0] (<= tau_0_t 1))))
(assert (or (and (= mode1_0 true) (= mode2_0 true) (= mode3_0 true)
                 (= [x1_0_t x2_0_t x3_0_t tau_0_t] 
                    (integral 0. time_0 [x1_0_0 x2_0_0 x3_0_0 tau_0_0] flow_1)))
            (and (= mode1_0 true) (= mode2_0 true) (= mode3_0 false)
                 (= [x1_0_t x2_0_t x3_0_t tau_0_t] 
                    (integral 0. time_0 [x1_0_0 x2_0_0 x3_0_0 tau_0_0] flow_2)))
            (and (= mode1_0 true) (= mode2_0 false) (= mode3_0 true)
                 (= [x1_0_t x2_0_t x3_0_t tau_0_t] 
                    (integral 0. time_0 [x1_0_0 x2_0_0 x3_0_0 tau_0_0] flow_3)))
            (and (= mode1_0 true) (= mode2_0 false) (= mode3_0 false)
                 (= [x1_0_t x2_0_t x3_0_t tau_0_t] 
                    (integral 0. time_0 [x1_0_0 x2_0_0 x3_0_0 tau_0_0] flow_4)))
            (and (= mode1_0 false) (= mode2_0 true) (= mode3_0 true)
                 (= [x1_0_t x2_0_t x3_0_t tau_0_t] 
                    (integral 0. time_0 [x1_0_0 x2_0_0 x3_0_0 tau_0_0] flow_5)))
            (and (= mode1_0 false) (= mode2_0 true) (= mode3_0 false)
                 (= [x1_0_t x2_0_t x3_0_t tau_0_t] 
                    (integral 0. time_0 [x1_0_0 x2_0_0 x3_0_0 tau_0_0] flow_6)))
            (and (= mode1_0 false) (= mode2_0 false) (= mode3_0 true)
                 (= [x1_0_t x2_0_t x3_0_t tau_0_t] 
                    (integral 0. time_0 [x1_0_0 x2_0_0 x3_0_0 tau_0_0] flow_7)))
            (and (= mode1_0 false) (= mode2_0 false) (= mode3_0 false)
                 (= [x1_0_t x2_0_t x3_0_t tau_0_t] 
                    (integral 0. time_0 [x1_0_0 x2_0_0 x3_0_0 tau_0_0] flow_8)))))
(assert (and (= tau_0_t 1) (= tau_1_0 0)))
(assert (and (= x1_1_0 x1_0_t)))
(assert (or (and (< x1_0_t 5) (= mode1_1 true))
            (and (>= x1_0_t 5) (= mode1_1 false))))
(assert (and (= x2_1_0 x2_0_t)))
(assert (or (and (< x2_0_t 5) (= mode2_1 true))
            (and (>= x2_0_t 5) (= mode2_1 false))))
(assert (and (= x3_1_0 x3_0_t)))
(assert (or (and (< x3_0_t 5) (= mode3_1 true))
            (and (>= x3_0_t 5) (= mode3_1 false))))
(assert (and (>= tau_1_0 0) (<= tau_1_0 1)
             (>= tau_1_t 0) (<= tau_1_t 1)
             (forall_t 1 [0 time_1] (>= tau_1_t 0))
             (forall_t 2 [0 time_1] (<= tau_1_t 1))))
(assert (or (and (= mode1_1 true) (= mode2_1 true) (= mode3_1 true)
                 (= [x1_1_t x2_1_t x3_1_t tau_1_t] 
                    (integral 0. time_1 [x1_1_0 x2_1_0 x3_1_0 tau_1_0] flow_1)))
            (and (= mode1_1 true) (= mode2_1 true) (= mode3_1 false)
                 (= [x1_1_t x2_1_t x3_1_t tau_1_t] 
                    (integral 0. time_1 [x1_1_0 x2_1_0 x3_1_0 tau_1_0] flow_2)))
            (and (= mode1_1 true) (= mode2_1 false) (= mode3_1 true)
                 (= [x1_1_t x2_1_t x3_1_t tau_1_t] 
                    (integral 0. time_1 [x1_1_0 x2_1_0 x3_1_0 tau_1_0] flow_3)))
            (and (= mode1_1 true) (= mode2_1 false) (= mode3_1 false)
                 (= [x1_1_t x2_1_t x3_1_t tau_1_t] 
                    (integral 0. time_1 [x1_1_0 x2_1_0 x3_1_0 tau_1_0] flow_4)))
            (and (= mode1_1 false) (= mode2_1 true) (= mode3_1 true)
                 (= [x1_1_t x2_1_t x3_1_t tau_1_t] 
                    (integral 0. time_1 [x1_1_0 x2_1_0 x3_1_0 tau_1_0] flow_5)))
            (and (= mode1_1 false) (= mode2_1 true) (= mode3_1 false)
                 (= [x1_1_t x2_1_t x3_1_t tau_1_t] 
                    (integral 0. time_1 [x1_1_0 x2_1_0 x3_1_0 tau_1_0] flow_6)))
            (and (= mode1_1 false) (= mode2_1 false) (= mode3_1 true)
                 (= [x1_1_t x2_1_t x3_1_t tau_1_t] 
                    (integral 0. time_1 [x1_1_0 x2_1_0 x3_1_0 tau_1_0] flow_7)))
            (and (= mode1_1 false) (= mode2_1 false) (= mode3_1 false)
                 (= [x1_1_t x2_1_t x3_1_t tau_1_t] 
                    (integral 0. time_1 [x1_1_0 x2_1_0 x3_1_0 tau_1_0] flow_8)))))
(assert (and (= tau_1_t 1) (= tau_2_0 0)))
(assert (and (= x1_2_0 x1_1_t)))
(assert (or (and (< x1_1_t 5) (= mode1_2 true))
            (and (>= x1_1_t 5) (= mode1_2 false))))
(assert (and (= x2_2_0 x2_1_t)))
(assert (or (and (< x2_1_t 5) (= mode2_2 true))
            (and (>= x2_1_t 5) (= mode2_2 false))))
(assert (and (= x3_2_0 x3_1_t)))
(assert (or (and (< x3_1_t 5) (= mode3_2 true))
            (and (>= x3_1_t 5) (= mode3_2 false))))
(assert (and (>= tau_2_0 0) (<= tau_2_0 1)
             (>= tau_2_t 0) (<= tau_2_t 1)
             (forall_t 1 [0 time_2] (>= tau_2_t 0))
             (forall_t 2 [0 time_2] (<= tau_2_t 1))))
(assert (or (and (= mode1_2 true) (= mode2_2 true) (= mode3_2 true)
                 (= [x1_2_t x2_2_t x3_2_t tau_2_t] 
                    (integral 0. time_2 [x1_2_0 x2_2_0 x3_2_0 tau_2_0] flow_1)))
            (and (= mode1_2 true) (= mode2_2 true) (= mode3_2 false)
                 (= [x1_2_t x2_2_t x3_2_t tau_2_t] 
                    (integral 0. time_2 [x1_2_0 x2_2_0 x3_2_0 tau_2_0] flow_2)))
            (and (= mode1_2 true) (= mode2_2 false) (= mode3_2 true)
                 (= [x1_2_t x2_2_t x3_2_t tau_2_t] 
                    (integral 0. time_2 [x1_2_0 x2_2_0 x3_2_0 tau_2_0] flow_3)))
            (and (= mode1_2 true) (= mode2_2 false) (= mode3_2 false)
                 (= [x1_2_t x2_2_t x3_2_t tau_2_t] 
                    (integral 0. time_2 [x1_2_0 x2_2_0 x3_2_0 tau_2_0] flow_4)))
            (and (= mode1_2 false) (= mode2_2 true) (= mode3_2 true)
                 (= [x1_2_t x2_2_t x3_2_t tau_2_t] 
                    (integral 0. time_2 [x1_2_0 x2_2_0 x3_2_0 tau_2_0] flow_5)))
            (and (= mode1_2 false) (= mode2_2 true) (= mode3_2 false)
                 (= [x1_2_t x2_2_t x3_2_t tau_2_t] 
                    (integral 0. time_2 [x1_2_0 x2_2_0 x3_2_0 tau_2_0] flow_6)))
            (and (= mode1_2 false) (= mode2_2 false) (= mode3_2 true)
                 (= [x1_2_t x2_2_t x3_2_t tau_2_t] 
                    (integral 0. time_2 [x1_2_0 x2_2_0 x3_2_0 tau_2_0] flow_7)))
            (and (= mode1_2 false) (= mode2_2 false) (= mode3_2 false)
                 (= [x1_2_t x2_2_t x3_2_t tau_2_t] 
                    (integral 0. time_2 [x1_2_0 x2_2_0 x3_2_0 tau_2_0] flow_8)))))
(assert (and (= tau_2_t 1) (= tau_3_0 0)))
(assert (and (= x1_3_0 x1_2_t)))
(assert (or (and (< x1_2_t 5) (= mode1_3 true))
            (and (>= x1_2_t 5) (= mode1_3 false))))
(assert (and (= x2_3_0 x2_2_t)))
(assert (or (and (< x2_2_t 5) (= mode2_3 true))
            (and (>= x2_2_t 5) (= mode2_3 false))))
(assert (and (= x3_3_0 x3_2_t)))
(assert (or (and (< x3_2_t 5) (= mode3_3 true))
            (and (>= x3_2_t 5) (= mode3_3 false))))
(assert (and (>= tau_3_0 0) (<= tau_3_0 1)
             (>= tau_3_t 0) (<= tau_3_t 1)
             (forall_t 1 [0 time_3] (>= tau_3_t 0))
             (forall_t 2 [0 time_3] (<= tau_3_t 1))))
(assert (or (and (= mode1_3 true) (= mode2_3 true) (= mode3_3 true)
                 (= [x1_3_t x2_3_t x3_3_t tau_3_t] 
                    (integral 0. time_3 [x1_3_0 x2_3_0 x3_3_0 tau_3_0] flow_1)))
            (and (= mode1_3 true) (= mode2_3 true) (= mode3_3 false)
                 (= [x1_3_t x2_3_t x3_3_t tau_3_t] 
                    (integral 0. time_3 [x1_3_0 x2_3_0 x3_3_0 tau_3_0] flow_2)))
            (and (= mode1_3 true) (= mode2_3 false) (= mode3_3 true)
                 (= [x1_3_t x2_3_t x3_3_t tau_3_t] 
                    (integral 0. time_3 [x1_3_0 x2_3_0 x3_3_0 tau_3_0] flow_3)))
            (and (= mode1_3 true) (= mode2_3 false) (= mode3_3 false)
                 (= [x1_3_t x2_3_t x3_3_t tau_3_t] 
                    (integral 0. time_3 [x1_3_0 x2_3_0 x3_3_0 tau_3_0] flow_4)))
            (and (= mode1_3 false) (= mode2_3 true) (= mode3_3 true)
                 (= [x1_3_t x2_3_t x3_3_t tau_3_t] 
                    (integral 0. time_3 [x1_3_0 x2_3_0 x3_3_0 tau_3_0] flow_5)))
            (and (= mode1_3 false) (= mode2_3 true) (= mode3_3 false)
                 (= [x1_3_t x2_3_t x3_3_t tau_3_t] 
                    (integral 0. time_3 [x1_3_0 x2_3_0 x3_3_0 tau_3_0] flow_6)))
            (and (= mode1_3 false) (= mode2_3 false) (= mode3_3 true)
                 (= [x1_3_t x2_3_t x3_3_t tau_3_t] 
                    (integral 0. time_3 [x1_3_0 x2_3_0 x3_3_0 tau_3_0] flow_7)))
            (and (= mode1_3 false) (= mode2_3 false) (= mode3_3 false)
                 (= [x1_3_t x2_3_t x3_3_t tau_3_t] 
                    (integral 0. time_3 [x1_3_0 x2_3_0 x3_3_0 tau_3_0] flow_8)))))
(assert (or (< x1_3_t (- 5 2)) (> x1_3_t (+ 5 2))))
(assert (or (< x2_3_t (- 5 2)) (> x2_3_t (+ 5 2))))
(assert (or (< x3_3_t (- 5 2)) (> x3_3_t (+ 5 2))))
(check-sat)
(exit)
