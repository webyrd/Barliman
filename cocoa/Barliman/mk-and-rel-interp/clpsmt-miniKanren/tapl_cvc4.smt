(set-logic QF_UFDTLIAFS)

(declare-datatypes ((L 0))
  (((zero) (succ (s1 L)) (plus (p1 L) (p2 L)) (ifz (i1 L) (i2 L) (i3 L)))))

(declare-const s (Set L))
(assert (member zero s))
(assert (member (succ zero) s))
(assert (not (= s (union (singleton zero) (singleton (succ zero))))))
(check-sat)
(get-model)

