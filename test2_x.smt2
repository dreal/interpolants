(set-logic QF_NRA_ODE)
(declare-fun y () Real)
(declare-fun x () Real)
(declare-fun y_0_0 () Real)
(declare-fun y_0_t () Real)
(declare-fun x_0_0 () Real)
(declare-fun x_0_t () Real)
(declare-fun time_0 () Real)
(declare-fun mode_0 () Real)
(define-ode flow_1 ((= d/dt[x] (+ (- 0 x) (sin x)))))
(assert (<= 0 y_0_0))
(assert (<= y_0_0 20))
(assert (<= 0 y_0_t))
(assert (<= y_0_t 20))
(assert (<= 0 x_0_0))
(assert (<= x_0_0 20))
(assert (<= 0 x_0_t))
(assert (<= x_0_t 20))
(assert (<= 0 time_0 [0.000000]))
(assert (<= time_0 3.5 [0.000000]))
(assert (<= 1 mode_0))
(assert (<= mode_0 1))
(assert (= mode_0 1))
(assert (= y_0_0 2))
(assert (= x_0_0 4))
(assert (= [x_0_t] (integral 0. time_0 [x_0_0] flow_1)))
(assert (> time_0 3.49))
(check-sat)
(exit)