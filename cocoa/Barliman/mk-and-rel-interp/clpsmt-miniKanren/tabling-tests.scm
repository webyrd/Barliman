(load "mk.scm")
(load "tabling.scm")
(load "z3-driver.scm")
(load "test-check.scm")

;; some tests inspired by
;; https://github.com/webyrd/tabling

(test "path-tabled"
  (letrec ((arc (lambda (x y)
                  (conde
                    ((== x 'a) (== y 'b))
                    ((== x 'b) (== y 'a))
                    ((== x 'b) (== y 'd)))))
           (path (tabled (x y)
                   (conde
                     ((arc x y))
                     ((fresh (z)
                        (arc x z)
                        (path z y)))))))
    (run* (q)
      (path 'a q)))
  '(b a d))


(test "facto-7"
  (letrec ((facto (tabled (n out)
                    (conde
                      ((z/assert `(= ,n 0))
                       (z/assert `(= ,out 1)))
                      ((z/assert `(not (= ,n 0)))
                       (fresh (n-1 r)
                         (z/assert `(= (- ,n 1) ,n-1))
                         (z/assert `(= (* ,n ,r) ,out))
                         (facto n-1 r)))))))
    (run 7 (q)
      (fresh (n out)
        (facto n out)
        (== q `(,n ,out)))))
  '((0 1) (1 1) (2 2) (3 6) (4 24) (5 120) (6 720)))

(test "facto-backwards-2"
  (letrec ((facto (tabled (n out)
                    (conde
                      ((z/assert `(= ,n 0))
                       (z/assert `(= ,out 1)))
                      ((z/assert `(not (= ,n 0)))
                       (fresh (n-1 r)
                         (z/assert `(= (- ,n 1) ,n-1))
                         (z/assert `(= (* ,n ,r) ,out))
                         (facto n-1 r)))))))
    (run* (q)
      (facto q 2)))
  '(2))

(test "facto-backwards-720"
  (letrec ((facto (tabled (n out)
                    (conde
                      ((z/assert `(= ,n 0))
                       (z/assert `(= ,out 1)))
                      ((z/assert `(not (= ,n 0)))
                       (fresh (n-1 r)
                         (z/assert `(= (- ,n 1) ,n-1))
                         (z/assert `(= (* ,n ,r) ,out))
                         (facto n-1 r)))))))
    (run* (q)
      (facto q 720)))
  '(6))

(test "fibo-1-non-tabled"
  (letrec ((fibo (lambda (n out)
                   (conde
                     ((z/assert `(= ,n 0))
                      (z/assert `(= ,out 0)))
                     ((z/assert `(= ,n 1))
                      (z/assert `(= ,out 1)))
                     ((z/assert `(> ,n 1))
                      (fresh (n-1 n-2 r1 r2)
                        (z/assert `(= (- ,n 1) ,n-1))
                        (z/assert `(= (- ,n 2) ,n-2))
                        (z/assert `(= (+ ,r1 ,r2) ,out))
                        (fibo n-1 r1)
                        (fibo n-2 r2)))))))
    (run 7 (q)
      (fresh (n out)
        (fibo n out)
        (== q `(,n ,out)))))
  '((0 0) (1 1) (2 1) (3 2) (4 3) (5 5) (6 8)))

(test "fibo-1-tabled"
  (letrec ((fibo (tabled (n out)
                   (conde
                     ((z/assert `(= ,n 0))
                      (z/assert `(= ,out 0)))
                     ((z/assert `(= ,n 1))
                      (z/assert `(= ,out 1)))
                     ((z/assert `(> ,n 1))
                      (fresh (n-1 n-2 r1 r2)
                        (z/assert `(= (- ,n 1) ,n-1))
                        (z/assert `(= (- ,n 2) ,n-2))
                        (fibo n-1 r1)
                        (fibo n-2 r2)
                        (z/assert `(= (+ ,r1 ,r2) ,out))))))))
    (run 7 (q)
      (fresh (n out)
        (fibo n out)
        (== q `(,n ,out)))))
  '((0 0) (1 1) (2 1) (3 2) (4 3) (5 5) (6 8)))
