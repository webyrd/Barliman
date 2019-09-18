(define z/set
  (lambda (s)
    (z/ `(declare-fun ,s () (Set Int)))))

(define subseto
  (lambda (r1 r2)
    (z/assert `(subset ,r1 ,r2))))

(define !subseto
  (lambda (r1 r2)
    (z/assert `(not (subset ,r1 ,r2)))))

(define set
  (lambda (s . args)
    `(insert ,@args ,s)))

(define ∅ '(as emptyset (Set Int)))

(define ino
  (lambda (x s)
    (z/assert `(member ,x ,s))))

(define !ino
  (lambda (x s)
    (z/assert `(not (member ,x ,s)))))

(define uniono
  (lambda (s1 s2 s3)
    (z/assert `(= (union ,s1 ,s2) ,s3))))

(define z/==
  (lambda (a b)
    (z/assert `(= ,a ,b))))
