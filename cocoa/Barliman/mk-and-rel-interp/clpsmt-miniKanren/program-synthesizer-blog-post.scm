(load "mk.scm")
(load "z3-driver.scm")
;;(load "cvc4-driver.scm")
(load "test-check.scm")
(load "interp-program-synthesizer-blog-post.scm")


;; Attempt to use miniKanren + SMT to solve synthesis problems described in the blog post:

;; Building a Program Synthesizer
;; James Bornholt
;; 10 July 2018
;;
;; https://homes.cs.washington.edu/~bornholt/post/building-synthesizer.html
;;
;; "Build a program synthesis tool, to generate programs from specifications, in 20 lines of code using Rosette."
;;
;; Code from the blog post is on GitHub?:
;;
;; https://gist.github.com/jamesbornholt/b51339fb8b348b53bfe8a5c66af66efe



;; Challenge 1: Find an integer whose absolute value is 5

;; Code from the blog post:

#|
#lang rosette/safe

; Compute the absolute value of `x`.
(define (absv x)
  (if (< x 0) (- x) x))

; Define a symbolic variable called y of type integer.
(define-symbolic y integer?)

; Solve a constraint saying |y| = 5.
(solve
  (assert (= (absv y) 5)))
|#

(test "challenge-1-a"
  (run* (y)
    (numbero y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (absv ',y))
           5))
  '(5 -5))

(test "challenge-1-b"
  (run* (y)
    (numbero y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (absv ,y))
           5))
  '(5 -5))

(test "challenge-1-c"
  (run* (y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (absv ',y))
           5))
  '(5 -5))

(test "challenge-1-d"
  (run 5 (y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (absv ,y))
           5))
  '(5 -5 '5 '-5 ((let ([_.0 _.1]) 5) (num _.1) (sym _.0))))



;; Challenge 2: Try to outsmart the solver by finding a 'y' whose
;; absolute value is less than 0.  There is no such 'y'.

#|
(solve (assert (< (absv y) 0)))
|#

(test "challenge-2-a"
  (run* (y)
    (numbero y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (< (absv ',y) 0))
           #t))
  '())

(test "challenge-2-b"
  (run* (y)
    (numbero y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (< (absv ,y) 0))
           #t))
  '())

(test "challenge-2-c"
  (run* (y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (< (absv ',y) 0))
           #t))
  '())

(test "challenge-2-d"
  (run* (y)
    (numbero y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (< (absv ',y) 1))
           #t))
  '(0))

(test "challenge-2-e"
  (run* (y)
    (numbero y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (< (absv ',y) 3))
           #t))
  '(0 2 1 -2 -1))


;; Challenge 3: Little interpreter for arithmetic

#|
(define (interpret p)
  (match p
    [(plus a b)  (+ (interpret a) (interpret b))]
    [(mul a b)   (* (interpret a) (interpret b))]
    [(square a)  (expt (interpret a) 2)]
    [_ p]))
|#

#|
(interpret (plus (square 7) 3)) => 52
|#


(define interpreto
  (lambda (p val)
    (fresh ()
      (numbero val)
      (conde
        [(numbero p) (== p val)]
        [(fresh (a n)
           (== `(square ,a) p)
           (numbero n)
           (z/assert `(= ,val (* ,n ,n)))
           (interpreto a n))]
        [(fresh (prim op a b n m)
           (== `(,prim ,a ,b) p)
           (conde
             [(== 'plus prim) (== '+ op)]
             [(== 'mul prim) (== '* op)])
           (numbero n)
           (numbero m)
           (z/assert `(= ,val (,op ,n ,m)))
           (interpreto a n)
           (interpreto b m))]))))

(test "challenge-3-interpreto-a"
  (run* (y)
    (interpreto '(plus (square 7) 3) y))
  '(52))


;; Using evalo:
(test "challenge-3-a"
  (run* (y)
    (numbero y)
    (evalo `(let ((plus (lambda (a b) (+ a b))))
              (let ((mul (lambda (a b) (* a b))))
                (let ((square (lambda (a) (* a a))))
                  (plus (square 7) 3))))
           y))
  '(52))

(test "challenge-3-b"
  (run* (y)
    (evalo `(let ((plus (lambda (a b) (+ a b))))
              (let ((mul (lambda (a b) (* a b))))
                (let ((square (lambda (a) (* a a))))
                  (plus (square 7) 3))))
           y))
  '(52))

(test "challenge-3-c"
  (run* (y)
    (numbero y)
    (evalo `(let ((square (lambda (a) (* a a))))
              (+ (square 7) 3))
           y))
  '(52))

(test "challenge-3-d"
  (run* (y)
    (evalo `(let ((square (lambda (a) (* a a))))
              (+ (square 7) 3))
           y))
  '(52))


;; Challenge 4: Find 'y' such that (square (plus y 2)) => 25

#|
(solve 
  (assert 
    (= (interpret (square (plus y 2))) 25)))
|#

(test "challenge-4-interpreto-a"
  (run* (y)
    (numbero y)
    (interpreto `(square (plus ,y 2)) 25))
  '(-7 3))

(test "challenge-4-interpreto-b"
  (run 2 (y)
    (interpreto `(square (plus ,y 2)) 25))
  '(-7 3))

(test "challenge-4-interpreto-c"
  (run 5 (y)
    (interpreto `(square (plus ,y 2)) 25))
  '(-7 3 (plus -7 0) (plus 1 -8) (plus 1 2)))


(test "challenge-4-a"
  (run* (y)
    (numbero y)
    (evalo `(let ((plus (lambda (a b) (+ a b))))
              (let ((mul (lambda (a b) (* a b))))
                (let ((square (lambda (a) (* a a))))
                  (square (+ ',y 2)))))
           25))
  '(-7 3))

(test "challenge-4-b"
  (run* (y)
    (numbero y)
    (evalo `(let ((plus (lambda (a b) (+ a b))))
              (let ((mul (lambda (a b) (* a b))))
                (let ((square (lambda (a) (* a a))))
                  (square (+ ,y 2)))))
           25))
  '(-7 3))

(test "challenge-4-c"
  (run* (y)
    (evalo `(let ((plus (lambda (a b) (+ a b))))
              (let ((mul (lambda (a b) (* a b))))
                (let ((square (lambda (a) (* a a))))
                  (square (+ ',y 2)))))
           25))
  '(-7 3))

(test "challenge-4-d"
  (run* (y)
    (numbero y)
    (evalo `(let ((square (lambda (a) (* a a))))
              (square (+ ',y 2)))
           25))
  '(-7 3))

(test "challenge-4-e"
  (run* (y)
    (numbero y)
    (evalo `(let ((square (lambda (a) (* a a))))
              (square (+ ,y 2)))
           25))
  '(-7 3))

(test "challenge-4-f"
  (run* (y)
    (evalo `(let ((square (lambda (a) (* a a))))
              (square (+ ',y 2)))
           25))
  '(-7 3))


;; Challenge 5: "find a constant c such that (mul c x) is equal to x + x for every possible x, rather than just a single x"

#|
(synthesize
  #:forall (list x)
  #:guarantee (assert (= (interpret (mul c x)) (+ x x))))
|#

(test "challenge-5-interpreto-a"
  (run 5 (q)
    (fresh (c x y two-x two-y)
      (== (list c x y) q)
      (numbero c)
      (numbero x)
      (numbero y)
      (=/= x y)
      (numbero two-x)
      (numbero two-y)
      (z/assert `(= ,two-x (+ ,x ,x)))
      (z/assert `(= ,two-y (+ ,y ,y)))
      (interpreto `(mul ,c ,x) two-x)
      (interpreto `(mul ,c ,y) two-y)))
  '((2 1 -1)
    (2 1 -2)
    (2 1 -3)
    (2 1 -4)
    (2 1 -5)))


;; oh dear!
;; is there a way for us to emulate Rosette's #:forall functionality?
(test "challenge-5-a"
  (run 10 (c)
    (fresh (x y)
      (numbero c)
      (numbero x)
      (numbero y)
      (=/= x y)
      (evalo `(and (equal? (* ,c ,x) (+ ,x ,x))
                   (equal? (* ,c ,y) (+ ,y ,y)))
             #t)))
  '(2 2 2 2 2 2 2 2 2 2))


;; Challenge 6:

;; we'd have to implement choose* in order to handle this example,
;; and would still need the universal quantification of challenge 5

(test "challenge-6-a"
  (run 5 (q)
    (fresh (body e1 e2 y z)
      (numbero y)
      (numbero z)
      (=/= y z)
      (== `(equal? (* 10 x) (+ ,e1 ,e2)) body)
      (== (list y body) q)
      (evalo `(let ((f (lambda (x) ,body)))
                (f ',y))
             #t)))
  '((0 (equal? (* 10 x) (+ 0 0)))
    (-1 (equal? (* 10 x) (+ 1 -11)))
    (-2 (equal? (* 10 x) (+ -1 -19)))
    (-3 (equal? (* 10 x) (+ 2 -32)))
    (1 (equal? (* 10 x) (+ -2 12)))))

(test "challenge-6-b"
  (run 5 (q)
    (fresh (body e1 e2 y z)
      (numbero y)
      (numbero z)
      (=/= y z)
      (== `(+ ,e1 ,e2) body)
      (== (list y body) q)
      (evalo `(let ((f (lambda (x) ,body)))
                (equal? (* 10 ',y) (f ',y)))
             #t)))
  '((0 (+ 0 0))
    (-1 (+ 1 -11))
    (-2 (+ -1 -19))
    (-3 (+ 2 -32))
    (1 (+ -2 12))))

(test "challenge-6-c"
  (run 1 (q)
    (fresh (body e1 e2 y z)
      (numbero y)
      (numbero z)
      (=/= y z)
      ;;
      (== e1 `(* x 8))
      (== e2 `(+ x x))
      ;;
      (== `(+ ,e1 ,e2) body)
      (== (list y z body) q)
      (evalo `(let ((f (lambda (x) ,body)))
                (list (equal? (* 10 ',y) (f ',y))
                      (equal? (* 10 ',z) (f ',z))))
             '(#t #t))))
  '((0 1 (+ (* x 8) (+ x x)))))

#!eof

(test "challenge-6-aa"
  (run* (q)
    (fresh (body e1 e2 y z)
      (numbero y)
      (numbero z)
      (=/= y z)
      (== `(+ ,e1 ,e2) body)
      (== (list y body) q)
      (evalo `(let ((plus (lambda (a b) (+ a b))))
                (let ((mul (lambda (a b) (* a b))))
                  (let ((square (lambda (a) (* a a))))
                    (let ((f (lambda (x) ,body)))
                      (equal? (mul 10 ',y) (f ',y))))))
             #t)))
  '?)

(test "challenge-6-d"
  (run 1 (q)
    (fresh (body e1 e2 y z)
      (numbero y)
      (numbero z)
      (=/= y z)
      (== `(equal? (* 10 x) (+ ,e1 ,e2)) body)
      (== (list y body) q)
      (evalo `(let ((f (lambda (x) ,body)))
                (list (f ',y) (f ',z)))
             '(#t #t))))
  '???)

(test "challenge-6-b"
  (run 1 (q)
    (fresh (e1 e2 y z)
      (numbero y)
      (numbero z)
      (=/= y z)
      (== `(+ ,e1 ,e2) q)
      (evalo `(let ((f (lambda (x) (equal? (* 10 x) ,q))))
                (and (f ',y) (f ',z)))
             #t)))
  '?)
