(load "mk.scm")
(load "z3-driver.scm")
(load "test-check.scm")

(define z/set
  (lambda (s)
    (z/ `(declare-fun ,s (Int) Bool))))

(define z/in
  (lambda (x s)
    (z/assert `(,s ,x))))

(define z/not-in
  (lambda (x s)
    (z/assert `(not (,s ,x)))))

(test "1"
  (run 1 (q)
    (fresh (s a b)
      (z/set s)
      (z/in a s)
      (z/not-in b s)
      (== q `(,s ,a ,b))))
  '(((lambda (x!0)
    (ite (= x!0 0) true (ite (= x!0 1) false true)))
   0
   1)))
