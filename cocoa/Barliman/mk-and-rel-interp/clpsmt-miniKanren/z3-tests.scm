(load "z3-driver.scm")
(load "test-check.scm")

(test "1"
  (check-sat
   '((declare-fun x () Int)
     (assert (>= x 0))))
  #t)

(test "2"
  (check-sat
   '((declare-fun x () Int)
     (assert (= x 0))
     (assert (= x 1))))
  #f)

(test "3"
  (get-model
   '((declare-fun x () Int)
     (assert (= x 0))))
  '((x . 0)))

