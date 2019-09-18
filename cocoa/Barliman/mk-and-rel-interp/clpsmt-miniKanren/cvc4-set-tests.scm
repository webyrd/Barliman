(load "mk.scm")
(load "cvc4-driver.scm")
(load "test-check.scm")

(test "set-1"
  (run* (q)
    (z/ `(declare-fun ,q () (Set Int)))
    (z/assert `(= ,q (singleton 1))))
  '((singleton 1)))

(test "set-2"
  (run* (q)
    (z/ `(declare-fun ,q () (Set Int)))
    (z/assert `(subset ,q (insert 2 (singleton 1)))))
  '((as emptyset (Set Int))
    (singleton 2)
    (singleton 1)
    (union [singleton 2] [singleton 1])))

(test "set-3"
  (run 4 (q)
    (z/ `(declare-fun ,q () (Set Int))))
  '((as emptyset (Set Int))
    (singleton 0)
    (singleton (- 1))
    (singleton (- 2))))

(test "set-4"
  (run* (q)
    (z/ `(declare-fun ,q () (Set Int)))
    (z/assert
     `(= (insert 1 2 (singleton 3))
         (union ,q (insert 1 (singleton 2))))))
  '((singleton 3)
    (union [singleton 1] [singleton 3])
    (union [singleton 2] [singleton 3])
    (union [union (singleton 1) (singleton 2)] [singleton 3])))

(load "clpset.scm")

(test "subseto-1"
  (run* (q)
    (subseto (set ∅ 1 2) (set ∅ 1 2 3)))
  '(_.0))

(test "subseto-1-not"
  (run* (q)
    (subseto (set ∅ 1 2 3) (set ∅ 1 2)))
  '())

(test "not-subseto-1"
  (run 1 (q)
    (!subseto (set ∅ 1 2 3) (set ∅ 1 2)))
  '(_.0))

(test "not-subseto-1-not"
  (run 1 (q)
    (!subseto (set ∅ 1 2) (set ∅ 1 2 3)))
  '())
