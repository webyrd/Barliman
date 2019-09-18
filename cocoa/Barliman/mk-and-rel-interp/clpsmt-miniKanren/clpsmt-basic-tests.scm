(test "counters"
  (let ((c1 z3-counter-check-sat)
        (c2 z3-counter-get-model))
    (run* (q)
      (z/assert `(= ,q 0)))
    (list
     (- z3-counter-check-sat c1)
     (- z3-counter-get-model c2)))
  '(4 1))

(test "declare-idempotent"
  (run* (q)
    (fresh (v1 v2)
      (z/ `(declare-const ,v1 Bool))
      (z/ `(declare-const ,v2 Bool))
      (== v1 v2)
      (== q v1)))
  '(_.0))

(test "inf-smt-ans-1"
  (run 1 (q)
    (z/assert `(>= ,q 0)))
  '(0))

(test "inf-smt-ans-2"
  (run 2 (q)
    (z/assert `(>= ,q 0)))
  '(0 1))

(test "1"
  (run* (q)
    (fresh (x)
      (z/assert `(= ,x 0))))
  '(_.0))

(test "2"
  (run* (q)
    (fresh (x)
      (z/assert `(= ,x 0))
      (z/assert `(= ,x 1))))
  '())

(test "3"
  (run* (q)
    (fresh (x)
     (z/assert `(= ,x 0))
     (== x q)))
  '(0))

(test "4"
  (run* (q)
    (z/assert `(= ,q 0)))
  '(0))

(test "5"
  (run 2 (f)
    (z/ `(declare-fun ,f (Int) Int))
    (z/assert `(= 1 (,f 1)))
    (z/assert `(= 0 (,f 0))))
  ;; TODO:
  ;; what do we really want here? syntax lambda or actual lambda?
  '((lambda (x!0) (ite (= x!0 1) 1 (ite (= x!0 0) 0 1)))))

(test "6"
  (run 1 (q)
    (fresh (f x)
      (z/ `(declare-fun ,f (Int) Int))
      (z/assert `(= ,x (,f ,x)))
      (== q x)))
  '(0))
