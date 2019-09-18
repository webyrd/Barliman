(set-logic ALL_SUPPORTED)

(declare-datatypes
 ((L 0))
 (((zero)
   (succ (pred L))
   (plus (a L) (b L))
   (ifz (is_zero L) (yes L) (no L)))))

(declare-fun x () L)
(declare-fun y () L)
(declare-fun z () L)
(assert (not (= (plus x y) z)))
(check-sat)
(get-model)
