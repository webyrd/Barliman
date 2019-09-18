(declare-fun x () Int)
(declare-fun y () Int)
(declare-fun z () Int)
(assert (>= (* 2 x) (+ y z)))
(assert (= x 0))
(assert (= y 1))
(assert (= z 1))
(check-sat) ; unsat
(exit)
