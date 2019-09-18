(load "mk.scm")
(load "cvc4-driver.scm")
(load "test-check.scm")

(define z/==
  (lambda (a b)
    (z/assert `(= ,a ,b))))

(define (L)
  (z/
   `(declare-datatypes
     ((L 0))
     (((zero)
       (succ (pred L))
       (plus (a L) (b L))
       (ifz (is_zero L) (yes L) (no L)))))))

(define (L/dec x)
  (z/ `(declare-const ,x L)))

(define (evalo l v)
  (conde
    ((z/== 'zero l) (z/== 0 v))
    ((fresh (x i)
       (L/dec x)
       (z/== `(succ ,x) l)
       (z/== `(+ 1 ,i) v)
       (evalo x i)))
    ((fresh (x y i j)
       (L/dec x)
       (L/dec y)
       (z/== `(plus ,x ,y) l)
       (z/== `(+ ,i ,j) v)
       (evalo x i)
       (evalo y j)))
    ((fresh (x y z i)
       (L/dec x)
       (L/dec y)
       (L/dec z)
       (z/== `(ifz ,x ,y ,z) l)
       (conde
         ((z/== i 0)
          (evalo y v))
         ((z/assert `(not (= ,i 0)))
          (evalo z v)))
       (evalo x i)))))

(test "evalo-0"
  (run* (q) (L) (evalo 'zero q))
  '(0))

(test "evalo-0-backwards"
  (run 2 (q) (L) (L/dec q) (evalo q 0))
  '(zero (plus zero zero)))

(test "evalo-1"
  (run* (q) (L) (evalo '(succ zero) q))
  '(1))

(test "evalo-1-backwards"
  (run 2 (q) (L) (L/dec q) (evalo q 1))
  '((succ zero) (succ (plus zero zero))))

(test "evalo-if"
  (run 1 (q) (L) (L/dec q) (evalo `(ifz ,q zero (succ zero)) 1))
  '((succ zero)))


