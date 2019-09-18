(load "mk.scm")
(load "cvc4-driver.scm")
(load "test-check.scm")

(define z/==
  (lambda (a b)
    (z/assert `(= ,a ,b))))

(define (L)
  (z/
   `(declare-datatypes
     ((L 0) (N 0))
     (((zero)
       (succ (pred L))
       (plus (a L) (b L))
       (ifz (is_zero L) (yes L) (no L)))
      ((z)
       (s (p N)))))))

(define (L/dec x)
  (z/ `(declare-const ,x L)))

(define (N/dec x)
  (z/ `(declare-const ,x N)))

(define (pluso n m o)
  (conde
    ((z/== 'z n) (z/== m o))
    ((fresh (n-1 o-1)
       (N/dec n-1)
       (N/dec o-1)
       (z/== `(s ,n-1) n)
       (z/== `(s ,o-1) o)
       (pluso n-1 m o-1)))))

(define (evalo l v)
  (conde
    ((z/== 'zero l) (z/== 'z v))
    ((fresh (x i)
       (L/dec x)
       (N/dec i)
       (z/== `(succ ,x) l)
       (z/== `(s ,i) v)
       (evalo x i)))
    ((fresh (x y i j)
       (L/dec x)
       (L/dec y)
       (N/dec i)
       (N/dec j)
       (z/== `(plus ,x ,y) l)
       (pluso i j v)
       (evalo x i)
       (evalo y j)))
    ((fresh (x y z i)
       (L/dec x)
       (L/dec y)
       (L/dec z)
       (N/dec i)
       (z/== `(ifz ,x ,y ,z) l)
       (conde
         ((z/== i 'z)
          (evalo y v))
         ((fresh (i-1)
            (N/dec i-1)
            (z/== `(s ,i-1) i)
            (evalo z v))))
       (evalo x i)))))

(test "evalo-0"
  (run* (q) (L) (N/dec q) (evalo 'zero q))
  '(z))

(test "evalo-0-backwards"
  (run 3 (q) (L) (L/dec q) (evalo q 'z))
  '(zero (plus zero zero) (ifz zero zero zero)))

(test "evalo-1"
  (run* (q) (L) (N/dec q) (evalo '(succ zero) q))
  '((s z)))

(test "evalo-1-backwards"
  (run 2 (q) (L) (L/dec q) (evalo q '(s z)))
  '((succ zero) (succ (plus zero zero))))

(test "evalo-if"
  (run 1 (q) (L) (L/dec q) (evalo `(ifz ,q zero (succ zero)) '(s z)))
  '((succ zero)))


