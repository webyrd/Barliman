;; CLP(SMT)
;; Nada Amin, University of Cambridge, UK
;; joint work with William E. Byrd, UAB, USA
;;
;; We show how easy it is to hook an SMT (Satisfiability Modulo Theory) solver such as CVC4 or Z3 as a backend to miniKanren,
;; an embedded domain-specific language in Scheme, to create an environment for constraint logic programming.
;; We describe the simple implementation and illustrate the technology through interpreters, synthesis, and symbolic execution.

(require "mk.rkt")
(load "test-check.scm")

;; crash course on miniKanren
(define appendo
  (lambda (l s ls)
    (conde
      [(== '() l) (== s ls)]
      [(fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) ls)
         (appendo d s res))])))

(test "appendo-forward"
  (run* (q)
    (appendo '(a b) '(c d e) q))
  '((a b c d e)))

(test "appendo-backward"
  (run* (q)
    (fresh (l s)
      (== q `(,l ,s))
      (appendo l s '(a b c d e))))
  '((() (a b c d e))
    ((a) (b c d e))
    ((a b) (c d e))
    ((a b c) (d e))
    ((a b c d) (e))
    ((a b c d e) ())))

(load "full-interp-quine.scm")

(test "evalo-append-forward"
  (run* (q)
    (evalo
     `(letrec ([append
                (lambda (l s)
                  (if (null? l)
                      s
                      (cons (car l) (append (cdr l) s))))])
        (append '(a b) '(c d e)))
     q))
  '((a b c d e)))

(test "evalo-append-backward"
  (run* (q)
    (fresh (s l)
      (== q `(,s ,l))
      (evalo
       `(letrec ([append
                  (lambda (l s)
                    (if (null? l)
                        s
                        (cons (car l) (append (cdr l) s))))])
          (append ',s ',l))
       '(a b c d e))))
  '((() (a b c d e))
    ((a) (b c d e))
    ((a b) (c d e))
    ((a b c) (d e))
    ((a b c d) (e))
    ((a b c d e) ())))

(test "evalo-quine"
  (run 4 (q) (evalo q q))
  '((_.0 (num _.0))
    #t
    #f
    (((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 closure))
          ((_.0 list))
          ((_.0 prim))
          ((_.0 quote)))
     (sym _.0))))

;; constraints
(test "numbero-0"
  (run* (q)
    (numbero q))
  '((_.0 (num _.0))))

(test "numbero-1"
  (run* (q)
    (numbero q)
    (== q 1))
  '(1))

(test "numbero-2"
  (run* (q)
    (numbero q)
    (== 'one q))
  '())

(test "symbolo-1"
  (run* (q)
    (symbolo q))
  '((_.0 (sym _.0))))

(test "absento-1"
  (run* (q)
    (absento 'one '(one two three)))
  '())

(test "absento-2"
  (run* (q)
    (absento 'one '(two three)))
  '(_.0))

;; CLP(SMT)

(test "basic-1"
  (run* (q)
    (z/assert `(> ,q 0))
    (z/assert `(< ,q 2)))
  '(1))

(test "basic-2"
  (run 3 (q)
    (fresh (x y)
      (== q `(,x ,y))
      (z/assert `(= ,x (+ ,y 1)))))
  '((1 0) (2 1) (3 2)))

(define faco
  (lambda (n out)
    (conde ((z/assert `(= ,n 0))
            (z/assert `(= ,out 1)))
           ((z/assert `(> ,n 0))
            (fresh (n-1 r)
              (z/assert `(= (- ,n 1) ,n-1))
              (z/assert `(= (* ,n ,r) ,out))
              (faco n-1 r))))))

;; equivalent
(define facto
  (lambda (n out)
    (conde ((== n 0)
            (== out 1))
           ((z/assert `(> ,n 0))
            (fresh (n-1 r)
              (z/assert `(= (- ,n 1) ,n-1))
              (z/assert `(= (* ,n ,r) ,out))
              (facto n-1 r))))))

(test "faco-7"
  (run 7 (q)
    (fresh (n out)
      (faco n out)
      (== q `(,n ,out))))
  '((0 1) (1 1) (2 2) (3 6) (4 24) (5 120) (6 720)))

(test "faco-backwards-2"
  (run* (q)
    (faco q 2))
  '(2))

(test "faco-backwards-720"
  (run* (q)
    (faco q 720))
  '(6))

;; equivalent
(test "facto-7"
  (run 7 (q)
    (fresh (n out)
      (facto n out)
      (== q `(,n ,out))))
  '((0 1) (1 1) (2 2) (3 6) (4 24) (5 120) (6 720)))

(test "facto-backwards-2"
  (run* (q)
    (facto q 2))
  '(2))

(test "facto-backwards-720"
  (run* (q)
    (facto q 720))
  '(6))

(load "full-interp.scm")

(test "evalo-1"
  (run* (q)
    (evalo '(+ 1 2) q))
  '(3))

(test "evalo-fac-6"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac 6))
           q))
  '(720))

(test "evalo-backwards-fac-6"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ,q))
           720))
  '(6))

;; remember the quote!
(test "evalo-backwards-fac-quoted-6"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ',q))
           720))
  '(6))

(test "symbolic-execution-2a"
  (run 10 (q)
    (evalo
      `((lambda (n)
          (if (= 137 n)
              'foo
              'bar))
        ,q)
      'bar))
  '(138 139 140 141 142 143 144 145 146 147))

(test "symbolic-execution-3a"
  (run* (q)
    (evalo
      `((lambda (n)
          (if (= (+ (* n 3) 5) 14359371734)
              'foo
              'bar))
        ',q)
      'foo))
  '(4786457243))

 (test "synthesize-triple-by-example-2c20c"
   (run 3 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op))
     (evalo `(list (,f 1) (,f 2)) '(2 4)))
   '((lambda (x) (+ x x))
     (lambda (x) (* 2 x))
     (lambda (x) (* x 2))))

(load "while-abort.scm")

;;; The following example is adapted from:
;;;
;;; https://github.com/webyrd/polyconf-2015/blob/master/talk-code/while-interpreter/while-abort-tests.scm

;;; symbolic execution example from slide 7 of Stephen Chong's slides
;;; on symbolic execution (contains contents from Jeff Foster's
;;; slides)
;;;
;;; http://www.seas.harvard.edu/courses/cs252/2011sp/slides/Lec13-SymExec.pdf

;;;  1. int a = α, b = β, c = γ
;;;  2.             // symbolic
;;;  3. int x = 0, y = 0, z = 0;
;;;  4. if (a) {
;;;  5.   x = -2;
;;;  6. }
;;;  7. if (b < 5) {
;;;  8.   if (!a && c)  { y = 1; }
;;;  9.   z = 2;
;;; 10. }
;;; 11. assert(x+y+z!=3)

;;; we will model the 'assert' using 'if' and 'abort'

;;; Slightly modified version that we are actually modelling:

;;;  1. int a := α, b := β, c := γ
;;;  4. if (a != 0) {
;;;  5.   x := -2;
;;;  6. }
;;;  7. if (b < 5) {
;;;  8.   if ((a = 0) && (c != 0))  { y := 1; }
;;;  9.   z := 2;
;;; 10. }
;;; 11. if (x+(y+z) != 3) {
;;;       abort
;;;     }

(define symbolic-exec-prog
  `(seq
     (if (!= a 0)
         (:= x -2)
         (skip))
     (seq
       (if (< b 5)
           (seq
             (if (and (= a 0) (!= c 0))
                 (:= y 1)
                 (skip))
             (:= z 2))
           (skip))
       (if (!= (+ x (+ y z)) 3)
           (skip)
           (abort)))))

(test "symbolic-exec-prog-c"
  (run 1 (q)
    (fresh (alpha beta gamma s)
      (== (list alpha beta gamma s) q)
      (z/assert `(not (= 0 ,alpha)))
      (z/assert `(<= 0 ,beta))
      (z/assert `(<= 0 ,gamma))
      (->o
       `(,symbolic-exec-prog
         ((a . ,alpha)
          (b . ,beta)
          (c . ,gamma)))
       `(abort ,s))))
  '())

(test "symbolic-exec-prog-d"
  (run 1 (q)
    (fresh (alpha beta gamma s)
      (== (list alpha beta gamma s) q)
      (z/assert `(not (= 0 ,alpha)))
      (->o
       `(,symbolic-exec-prog
         ((a . ,alpha)
          (b . ,beta)
          (c . ,gamma)))
       `(abort ,s))))
  '())

(test "symbolic-exec-prog-e"
  (run 8 (q)
    (fresh (alpha beta gamma s)
      (== (list alpha beta gamma s) q)
      (z/assert `(not (= 0 ,beta)))
      (->o
       `(,symbolic-exec-prog
         ((a . ,alpha)
          (b . ,beta)
          (c . ,gamma)))
       `(abort ,s))))
  '((0 1 1 ((z . 2) (y . 1) (a . 0) (b . 1) (c . 1)))
    (0 -1 -1 ((z . 2) (y . 1) (a . 0) (b . -1) (c . -1)))
    (0 -2 -2 ((z . 2) (y . 1) (a . 0) (b . -2) (c . -2)))
    (0 -3 -3 ((z . 2) (y . 1) (a . 0) (b . -3) (c . -3)))
    (0 -4 -4 ((z . 2) (y . 1) (a . 0) (b . -4) (c . -4)))
    (0 -5 -5 ((z . 2) (y . 1) (a . 0) (b . -5) (c . -5)))
    (0 -6 -6 ((z . 2) (y . 1) (a . 0) (b . -6) (c . -6)))
    (0 2 -7 ((z . 2) (y . 1) (a . 0) (b . 2) (c . -7)))))




(test "lool"
  (run 4 (f)
    (evalo `(list (,f 1) (,f 2) (,f 3)) '(1 2 3)))
  '(quote and or ((lambda (_.0) _.0) (sym _.0))))

(test "f"
  (run 1 (f)
    (fresh (e)
      (== `(lambda (x) ,e) f)
      (absento 'match e))
    (evalo `(list (,f 1) (,f 2) (,f 3)) '(173 174 175)))
  '((lambda (x) (+ 172 x))))

(test "g"
  (run 1 (f)
    (fresh (e)
      (== `(lambda (x) ,e) f)
      (absento 'match e))
    (evalo `(list (,f 0) (,f 1) (,f 2)) '(0 2 4)))
  '((lambda (x) (+ x x))))

(test "h-c"
  (run 1 (f)
    (fresh (e)
      (== `(lambda (x) ,e) f)
      (absento 'match e))
    (evalo `(list (,f 2) (,f 3) (,f 4)) '(274 411 548)))
  '((lambda (x) (* 137 x))))
