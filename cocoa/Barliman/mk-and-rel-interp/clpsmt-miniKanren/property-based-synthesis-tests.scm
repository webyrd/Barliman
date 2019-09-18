;(load "mk.scm")
;(load "z3-driver.scm")
;(load "test-check.scm")
(load "../clpsmt-miniKanren/full-interp-extended.scm")


(test "synthesize-triple-by-example-2c13"
  (run 1 (f)
    (fresh (op e1 e2)
      (== `(lambda (x) (,op ,e1 ,e2)) f)
      (symbolo op)
      (numbero e1)
      (symbolo e2))
    (evalo `(list (,f 1) (,f 2)) '(3 6)))
  '((lambda (x) (* 3 x))))

(time-test "synthesize-triple-by-example-2c14"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op)
       (numbero e1))
     (evalo `(list (,f 1) (,f 2)) '(3 6)))
   '((lambda (x) (* 3 x))))

(time-test "synthesize-triple-by-example-2c15"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op))
     (evalo `(list (,f 1)) '(3)))
   '(((lambda (x) (let ((_.0 _.1)) 3)) (num _.1) (sym _.0))))

(time-test "synthesize-triple-by-example-2c16"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op))
     (evalo `(list (,f 1) (,f 1)) '(3 3)))
   '(((lambda (x) (let ((_.0 _.1)) 3)) (num _.1) (sym _.0))))

(time-test "synthesize-triple-by-example-2c17"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op))
     (evalo `(list (,f 1) (,f 2)) '(1 2)))
   '(((lambda (x) (let ((_.0 _.1)) x)) (=/= ((_.0 x))) (num _.1) (sym _.0))))

(time-test "synthesize-triple-by-example-2c18"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op))
     (evalo `(list (,f 1) (,f 2)) '(2 3)))
   '((lambda (x) (+ 1 x))))

(time-test "synthesize-triple-by-example-2c19"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op))
     (evalo `(list (,f 1) (,f 2)) '(18 19)))
   '((lambda (x) (+ 17 x))))

(time-test "synthesize-triple-by-example-2c20a"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op)
       (== '* op))
     (evalo `(list (,f 1) (,f 2)) '(2 4)))
   '((lambda (x) (* 2 x))))

(time-test "synthesize-triple-by-example-2c20b"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op)
       (=/= 'let op)
       (=/= 'letrec op))
     (evalo `(list (,f 1) (,f 2)) '(2 4)))
   '((lambda (x) (+ x x))))

(time-test "synthesize-triple-by-example-2c20c"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op))
     (evalo `(list (,f 1) (,f 2)) '(2 4)))
   '((lambda (x) (+ x x))))

(time-test "synthesize-triple-by-example-2c21a"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op)
       (=/= 'let op)
       (=/= 'letrec op))
     (evalo `(list (,f 1) (,f 2)) '(3 6)))
   '((lambda (x) (* 3 x))))

(time-test "synthesize-triple-by-example-2c21b"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op)
       (=/= 'let op))
     (evalo `(list (,f 1) (,f 2)) '(3 6)))
   '((lambda (x) (* 3 x))))

(time-test "synthesize-triple-by-example-2c21c"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op)
       (conde
         [(== 'let op)]
         [(== '* op)]))
     (evalo `(list (,f 1) (,f 2)) '(3 6)))
   '((lambda (x) (* 3 x))))

(time-test "synthesize-triple-by-example-2c21d"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op)
       (=/= 'letrec op))
     (evalo `(list (,f 1) (,f 2)) '(3 6)))
   '((lambda (x) (* 3 x))))

(time-test "synthesize-triple-by-example-2c21e"
   (run 1 (f)
     (fresh (op e1 e2)
       (== `(lambda (x) (,op ,e1 ,e2)) f)
       (symbolo op))
     (evalo `(list (,f 1) (,f 2)) '(3 6)))
   '((lambda (x) (* 3 x))))




(test "synthesize-triple-by-property-1a"
  (run 1 (f)
    (fresh (e x x-tripled)
      (== `(lambda (x) ,e) f)
      (evalo `(,f ,x) x-tripled)))
  '(((lambda (x) _.0) (num _.0))))

;; too bad!  we get existential, not universal, quantification
(test "synthesize-triple-by-property-1b"
  (run 1 (q)
    (fresh (f x x-tripled e)
      (== (list f x x-tripled) q)
      (== `(lambda (x) ,e) f)
      (numbero x)
      (numbero x-tripled)
      (z/assert `(= ,x (* 3 ,x-tripled)))
      (evalo `(,f ,x) x-tripled)))
  '(((lambda (x) 0) 0 0)))


(test "synthesize-triple-by-example-1a"
  (run 1 (e)
    (evalo `(,e 0) 0))
  '(quote))


(test "synthesize-triple-by-example-1b"
  (run 1 (e)
    (evalo `(,e 0) 0)
    (evalo `(,e 1) 3))
  '(((lambda (_.0) (match _.0 (0 0) (1 3) . _.1)) (=/= ((_.0 match))) (sym _.0))))


(test "synthesize-triple-by-example-2a"
  (run 1 (f)
    (fresh (e)
      (== `(lambda (x) ,e) f))
    (evalo `(,f 0) 0))
  '((lambda (x) 0)))

(test "synthesize-triple-by-example-2b"
  (run 1 (f)
    (fresh (e)
      (== `(lambda (x) ,e) f))
    (evalo `(,f 1) 3))
  '((lambda (x) 3)))


(test "synthesize-triple-by-example-2c"
  (run 1 (f)
    (fresh (e)
      (== `(lambda (x) ,e) f))
    (evalo `(cons (,f 0) (,f 1)) '(0 . 3)))
  '((lambda (x) (match x (0 0) (1 3) . _.0))))


(test "synthesize-triple-by-example-2c1"
  (run 1 (f)
    (fresh (e)
      (== `(lambda (x) ,e) f)
      (== `(lambda (x) (* x 3)) f))
    (evalo `(cons (,f 0) (,f 1)) '(0 . 3)))
  '((lambda (x) (* x 3))))

(test "synthesize-triple-by-example-2c2"
  (run 1 (f)
    (fresh (e expr)
      (== `(lambda (x) ,e) f)
      (== `(lambda (x) (* . ,expr)) f))
    (evalo `(cons (,f 0) (,f 1)) '(0 . 3)))
  '((lambda (x) (* 3 x))))

(test "synthesize-triple-by-example-2c3"
  (run 1 (f)
    (fresh (expr)
      (== `(lambda (x) (* . ,expr)) f))
    (evalo `(cons (,f 0) (,f 1)) '(0 . 3)))
  '((lambda (x) (* 3 x))))

(test "synthesize-triple-by-example-2c4"
  (run 1 (f)
    (fresh (op args)
      (== `(lambda (x) (,op . ,args)) f)
      (== '* op))
    (evalo `(cons (,f 0) (,f 1)) '(0 . 3)))
  '((lambda (x) (* 3 x))))

(test "synthesize-triple-by-example-2c5"
  (run 1 (f)
    (fresh (op args)
      (== `(lambda (x) (,op . ,args)) f)
      (conde
        [(== '+ op)]
        [(== '- op)]
        [(== '/ op)]
        [(== '* op)]))
    (evalo `(cons (,f 0) (,f 1)) '(0 . 3)))
  '((lambda (x) (* 3 x))))

(test "synthesize-triple-by-example-2c6"
  (run 1 (f)
    (fresh (op args)
      (== `(lambda (x) (,op . ,args)) f)
      (conde
        [(== '= op)]
        [(== '> op)]
        [(== '>= op)]
        [(== '< op)]
        [(== '<= op)]
        [(== '!= op)]
        [(== '+ op)]
        [(== '- op)]
        [(== '/ op)]
        [(== '* op)]))
    (evalo `(cons (,f 0) (,f 1)) '(0 . 3)))
  '((lambda (x) (* 3 x))))

(test "synthesize-triple-by-example-2c7"
  (run 1 (f)
    (fresh (op args)
      (== `(lambda (x) (,op . ,args)) f)
      (symbolo op))
    (evalo `(cons (,f 0) (,f 1)) '(0 . 3)))
  '((lambda (x) (match x (0 0) (1 3) . _.0))))

(test "synthesize-triple-by-example-2c8"
  (run 1 (f)
    (fresh (op args)
      (== `(lambda (x) (,op . ,args)) f)
      (symbolo op))
    (evalo `(list (,f 1) (,f 2) (,f 3) (,f 4)) '(3 6 9 12)))
  '((lambda (x) (match x (1 3) (2 6) (3 9) (4 12) . _.0))))

(test "synthesize-triple-by-example-2c9"
  (run 1 (f)
    (fresh (op args)
      (== `(lambda (x) (,op . ,args)) f)
      (symbolo op)
      (=/= 'match op))
    (evalo `(,f 2) 6))
  '((lambda (x) '6)))

(test "synthesize-triple-by-example-2c10"
  (run 1 (f)
    (fresh (op args)
      (== `(lambda (x) (,op . ,args)) f)
      (symbolo op)
      (=/= 'match op))
    (evalo `(list (,f 2)) '(6)))
  '((lambda (x) '6)))



(test "synthesize-triple-by-example-2c11"
  (run 1 (f)
    (fresh (op args)
      (== `(lambda (x) (,op . ,args)) f)
      (symbolo op)
      (=/= 'match op))
    (evalo `(list (,f 1) (,f 2)) '(3 6)))
  '((lambda (x) (* 3 x))))

(test "synthesize-triple-by-example-2c12"
  (run 1 (f)
    (fresh (op args)
      (== `(lambda (x) (,op . ,args)) f)
      (symbolo op)
      (=/= 'match op))
    (evalo `(list (,f 1) (,f 2) (,f 3)) '(3 6 9)))
  '((lambda (x) (* 3 x))))

(test "synthesize-triple-by-example-2c5"
  (run 1 (f)
    (fresh (op args)
      (== `(lambda (x) (,op . ,args)) f))
    (evalo `(cons (,f 0) (,f 1)) '(0 . 3)))
  '((lambda (x) (match x (0 0) (1 3) . _.0))))

(test "synthesize-triple-by-example-2d"
  (run 1 (f)
    (fresh (e)
      (== `(lambda (x) ,e) f))
    (evalo `(,f 0) 0)
    (evalo `(,f 1) 3))
  '((lambda (x) (match x (0 0) (1 3) . _.0))))





;; can we prove that fact of any number greater than 1 must be odd?

;; standard concrete execution
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


(test "evalo-fac-odd-n-1"
  (run 1 (q)
    (fresh (x y z)
      (== (list x y z) q)
      (numbero x)
      (numbero y)
      (numbero z)
      (z/assert `(<= 0 ,y))
      (z/assert `(<= 0 ,x))
      (z/assert `(<= 0 ,z))
      ;; assert z is odd
      (z/assert `(= ,z (+ (* 2 ,y) 1)))
      (evalo `(letrec ((fac
                        (lambda (n)
                          (if (< n 0) #f
                              (if (= n 0) 1
                                  (* n (fac (- n 1))))))))
                (fac ,x))
             z)))
  '((0 0 1)))

(test "evalo-fac-odd-n-2"
  (run 2 (q)
    (fresh (x y z)
      (== (list x y z) q)
      (numbero x)
      (numbero y)
      (numbero z)
      (z/assert `(<= 0 ,y))
      (z/assert `(<= 0 ,x))
      (z/assert `(<= 0 ,z))
      ;; assert z is odd
      (z/assert `(= ,z (+ (* 2 ,y) 1)))
      (evalo `(letrec ((fac
                        (lambda (n)
                          (if (< n 0) #f
                              (if (= n 0) 1
                                  (* n (fac (- n 1))))))))
                (fac ,x))
             z)))
  '((0 0 1) (1 0 1)))

;; terminates!!
;;
;; proof that for any non-negative integer input, the only odd output
;; factorial can produce is '1'
(test "evalo-fac-odd-n-3"
  (run* (q)
    (fresh (x y z)
      (== (list x y z) q)
      (numbero x)
      (numbero y)
      (numbero z)
      (z/assert `(<= 0 ,y))
      (z/assert `(<= 0 ,x))
      (z/assert `(<= 0 ,z))
      ;; assert z is odd
      (z/assert `(= ,z (+ (* 2 ,y) 1)))
      (evalo `(letrec ((fac
                        (lambda (n)
                          (if (< n 0) #f
                              (if (= n 0) 1
                                  (* n (fac (- n 1))))))))
                (fac ,x))
             z)))
  '((0 0 1) (1 0 1)))

;; even version of the above factorial code
(test "evalo-fac-even-n-3"
  (run 3 (q)
    (fresh (x y z)
      (== (list x y z) q)
      (numbero x)
      (numbero y)
      (numbero z)
      (z/assert `(<= 0 ,y))
      (z/assert `(<= 0 ,x))
      (z/assert `(<= 0 ,z))
      ;; assert z is even
      (z/assert `(= ,z (* 2 ,y)))
      (evalo `(letrec ((fac
                        (lambda (n)
                          (if (< n 0) #f
                              (if (= n 0) 1
                                  (* n (fac (- n 1))))))))
                (fac ,x))
             z)))
  '((2 1 2) (3 3 6) (4 12 24)))


(test "evalo-simple-let-a"
  (run* (q)
    (evalo '(let ((foo (+ 1 2))) (* foo foo)) q))
  '(9))


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

(test "evalo-symbolic-execution-a"
  (run 1 (q)
    (fresh (alpha beta gamma)
      (== (list alpha beta gamma) q)
      (evalo `(let ((a ',alpha))
                (let ((b ',beta))
                  (let ((c ',gamma))
                    (let ((x (if (!= a 0)
                                 -2
                                 0)))
                      (let ((y (if (and (< b 5) (= a 0) (!= c 0))
                                   1
                                   0)))
                        (let ((z (if (< b 5)
                                     2
                                     0)))
                          (if (!= (+ x (+ y z)) 3)
                              'good
                              'bad)))))))
             'bad)))  
  '((0 4 1)))

(test "evalo-symbolic-execution-b"
  (run 8 (q)
    (fresh (alpha beta gamma)
      (== (list alpha beta gamma) q)
      (evalo `(let ((a ',alpha))
                (let ((b ',beta))
                  (let ((c ',gamma))
                    (let ((x (if (!= a 0)
                                 -2
                                 0)))
                      (let ((y (if (and (< b 5) (= a 0) (!= c 0))
                                   1
                                   0)))
                        (let ((z (if (< b 5)
                                     2
                                     0)))
                          (if (!= (+ x (+ y z)) 3)
                              'good
                              'bad)))))))
             'bad)))  
  '((0 4 1)
    (0 0 -1)
    (0 -1 -2)
    (0 -2 -3)
    (0 -3 -4)
    (0 -4 -5)
    (0 -5 -6)
    (0 -6 -7)))


(test "evalo-symbolic-execution-c"
  (run 8 (q)
    (fresh (alpha beta gamma vals)
      (== (list alpha beta gamma vals) q)
      (evalo `(let ((a ',alpha))
                (let ((b ',beta))
                  (let ((c ',gamma))
                    (let ((x (if (!= a 0)
                                 -2
                                 0)))
                      (let ((y (if (and (< b 5) (= a 0) (!= c 0))
                                   1
                                   0)))
                        (let ((z (if (< b 5)
                                     2
                                     0)))
                          (if (!= (+ x (+ y z)) 3)
                              'good
                              (list 'bad x y z))))))))
             `(bad . ,vals))))  
  '((0 4 1 (0 1 2))
    (0 0 -1 (0 1 2))
    (0 -1 -2 (0 1 2))
    (0 -2 -3 (0 1 2))
    (0 -3 -4 (0 1 2))
    (0 -4 -5 (0 1 2))
    (0 -5 -6 (0 1 2))
    (0 -6 -7 (0 1 2))))

(test "evalo-symbolic-execution-d"
  (run 1 (q)
    (fresh (alpha beta gamma vals)
      (z/assert `(not (= 0 ,beta)))
      (== (list alpha beta gamma vals) q)
      (evalo `(let ((a ',alpha))
                (let ((b ',beta))
                  (let ((c ',gamma))
                    (let ((x (if (!= a 0)
                                 -2
                                 0)))
                      (let ((y (if (and (< b 5) (= a 0) (!= c 0))
                                   1
                                   0)))
                        (let ((z (if (< b 5)
                                     2
                                     0)))
                          (if (!= (+ x (+ y z)) 3)
                              'good
                              (list 'bad x y z))))))))
             `(bad . ,vals))))  
  '((0 1 1 (0 1 2))))

(test "evalo-symbolic-execution-e"
  (run 1 (q)
    (fresh (alpha beta gamma vals)
      (z/assert `(not (= 0 ,alpha)))
      (== (list alpha beta gamma vals) q)
      (evalo `(let ((a ',alpha))
                (let ((b ',beta))
                  (let ((c ',gamma))
                    (let ((x (if (!= a 0)
                                 -2
                                 0)))
                      (let ((y (if (and (< b 5) (= a 0) (!= c 0))
                                   1
                                   0)))
                        (let ((z (if (< b 5)
                                     2
                                     0)))
                          (if (!= (+ x (+ y z)) 3)
                              'good
                              (list 'bad x y z))))))))
             `(bad . ,vals))))
  '())

;;;

(test "evalo-symbolic-execution-f"
  (run 8 (q)
    (fresh (alpha beta gamma vals)
      (== (list alpha beta gamma vals) q)
      (evalo `((lambda (a b c)
                 (let ((x (if (!= a 0)
                              -2
                              0)))
                   (let ((y (if (and (< b 5) (= a 0) (!= c 0))
                                1
                                0)))
                     (let ((z (if (< b 5)
                                  2
                                  0)))
                       (if (!= (+ x (+ y z)) 3)
                           'good
                           (list 'bad x y z))))))
               ',alpha ',beta ',gamma)
             `(bad . ,vals))))  
  '((0 4 1 (0 1 2))
    (0 0 -1 (0 1 2))
    (0 -1 -2 (0 1 2))
    (0 -2 -3 (0 1 2))
    (0 -3 -4 (0 1 2))
    (0 -4 -5 (0 1 2))
    (0 -5 -6 (0 1 2))
    (0 -6 -7 (0 1 2))))

(test "evalo-symbolic-execution-g"
  (run 8 (q)
    (fresh (alpha beta gamma vals)
      (z/assert `(not (= 0 ,beta)))
      (== (list alpha beta gamma vals) q)
      (evalo `((lambda (a b c)
                 (let ((x (if (!= a 0)
                              -2
                              0)))
                   (let ((y (if (and (< b 5) (= a 0) (!= c 0))
                                1
                                0)))
                     (let ((z (if (< b 5)
                                  2
                                  0)))
                       (if (!= (+ x (+ y z)) 3)
                           'good
                           (list 'bad x y z))))))
               ',alpha ',beta ',gamma)
             `(bad . ,vals))))  
  '((0 1 1 (0 1 2))
    (0 -1 -1 (0 1 2))
    (0 -2 -2 (0 1 2))
    (0 -3 -3 (0 1 2))
    (0 -4 -4 (0 1 2))
    (0 -5 -5 (0 1 2))
    (0 -6 -6 (0 1 2))
    (0 2 -7 (0 1 2))))

;; run* terminates with these restrictions on alpha, beta gamma
(test "evalo-symbolic-execution-g2"
  (run* (q)
    (fresh (alpha beta gamma vals)
      (z/assert `(<= 0 ,alpha))
      (z/assert `(<= 0 ,beta))
      (z/assert `(<= 0 ,gamma))
      (z/assert `(<= ,gamma 10))
      (== (list alpha beta gamma vals) q)
      (evalo `((lambda (a b c)
                 (let ((x (if (!= a 0)
                              -2
                              0)))
                   (let ((y (if (and (< b 5) (= a 0) (!= c 0))
                                1
                                0)))
                     (let ((z (if (< b 5)
                                  2
                                  0)))
                       (if (!= (+ x (+ y z)) 3)
                           'good
                           (list 'bad x y z))))))
               ',alpha ',beta ',gamma)
             `(bad . ,vals))))
  '((0 0 1 (0 1 2))
    (0 1 1 (0 1 2))
    (0 2 2 (0 1 2))
    (0 3 3 (0 1 2))
    (0 4 4 (0 1 2))
    (0 4 5 (0 1 2))
    (0 4 6 (0 1 2))
    (0 4 7 (0 1 2))
    (0 4 8 (0 1 2))
    (0 4 9 (0 1 2))
    (0 4 10 (0 1 2))
    (0 4 3 (0 1 2))
    (0 3 6 (0 1 2))
    (0 3 10 (0 1 2))
    (0 3 7 (0 1 2))
    (0 3 8 (0 1 2))
    (0 3 9 (0 1 2))
    (0 3 4 (0 1 2))
    (0 3 5 (0 1 2))
    (0 4 2 (0 1 2))
    (0 3 2 (0 1 2))
    (0 2 4 (0 1 2))
    (0 4 1 (0 1 2))
    (0 2 1 (0 1 2))
    (0 3 1 (0 1 2))
    (0 2 3 (0 1 2))
    (0 2 6 (0 1 2))
    (0 2 5 (0 1 2))
    (0 2 7 (0 1 2))
    (0 2 8 (0 1 2))
    (0 2 10 (0 1 2))
    (0 2 9 (0 1 2))
    (0 1 10 (0 1 2))
    (0 1 2 (0 1 2))
    (0 1 3 (0 1 2))
    (0 1 5 (0 1 2))
    (0 1 6 (0 1 2))
    (0 1 4 (0 1 2))
    (0 1 7 (0 1 2))
    (0 1 8 (0 1 2))
    (0 1 9 (0 1 2))
    (0 0 10 (0 1 2))
    (0 0 4 (0 1 2))
    (0 0 2 (0 1 2))
    (0 0 6 (0 1 2))
    (0 0 5 (0 1 2))
    (0 0 3 (0 1 2))
    (0 0 7 (0 1 2))
    (0 0 8 (0 1 2))
    (0 0 9 (0 1 2))))

(test "evalo-symbolic-execution-h"
  (run* (q)
    (fresh (alpha beta gamma vals)
      (z/assert `(not (= 0 ,alpha)))
      (== (list alpha beta gamma vals) q)
      (evalo `((lambda (a b c)
                 (let ((x (if (!= a 0)
                              -2
                              0)))
                   (let ((y (if (and (< b 5) (= a 0) (!= c 0))
                                1
                                0)))
                     (let ((z (if (< b 5)
                                  2
                                  0)))
                       (if (!= (+ x (+ y z)) 3)
                           'good
                           (list 'bad x y z))))))
               ',alpha ',beta ',gamma)
             `(bad . ,vals))))  
  '())

;;;

(test "evalo-symbolic-execution-i"
  (run 8 (q)
    (fresh (alpha beta gamma vals)
      (== (list alpha beta gamma vals) q)
      (evalo `((lambda (a b c)
                 ((lambda (x y z)
                    (if (!= (+ x (+ y z)) 3)
                        'good
                        (list 'bad x y z)))
                  ;; x
                  (if (!= a 0)
                      -2
                      0)
                  ;; y
                  (if (and (< b 5) (= a 0) (!= c 0))
                      1
                      0)
                  ;; z
                  (if (< b 5)
                      2
                      0)))
               ',alpha ',beta ',gamma)
             `(bad . ,vals))))
  '((0 4 1 (0 1 2))
    (0 0 -1 (0 1 2))
    (0 -1 -2 (0 1 2))
    (0 -2 -3 (0 1 2))
    (0 -3 -4 (0 1 2))
    (0 -4 -5 (0 1 2))
    (0 -5 -6 (0 1 2))
    (0 -6 -7 (0 1 2))))

(test "evalo-symbolic-execution-j"
  (run 8 (q)
    (fresh (alpha beta gamma vals)
      (z/assert `(not (= 0 ,beta)))
      (== (list alpha beta gamma vals) q)
      (evalo `((lambda (a b c)
                 ((lambda (x y z)
                    (if (!= (+ x (+ y z)) 3)
                        'good
                        (list 'bad x y z)))
                  ;; x
                  (if (!= a 0)
                      -2
                      0)
                  ;; y
                  (if (and (< b 5) (= a 0) (!= c 0))
                      1
                      0)
                  ;; z
                  (if (< b 5)
                      2
                      0)))
               ',alpha ',beta ',gamma)
             `(bad . ,vals))))
  '((0 1 1 (0 1 2))
    (0 -1 -1 (0 1 2))
    (0 -2 -2 (0 1 2))
    (0 -3 -3 (0 1 2))
    (0 -4 -4 (0 1 2))
    (0 -5 -5 (0 1 2))
    (0 -6 -6 (0 1 2))
    (0 2 -7 (0 1 2))))

(test "evalo-symbolic-execution-k"
  (run* (q)
    (fresh (alpha beta gamma vals)
      (z/assert `(not (= 0 ,alpha)))
      (== (list alpha beta gamma vals) q)
      (evalo `((lambda (a b c)
                 ((lambda (x y z)
                    (if (!= (+ x (+ y z)) 3)
                        'good
                        (list 'bad x y z)))
                  ;; x
                  (if (!= a 0)
                      -2
                      0)
                  ;; y
                  (if (and (< b 5) (= a 0) (!= c 0))
                      1
                      0)
                  ;; z
                  (if (< b 5)
                      2
                      0)))
               ',alpha ',beta ',gamma)
             `(bad . ,vals))))
  '())




(time-test "a"
    (run 1 (f)
      (fresh (e)
        (== `(lambda (x) ,e) f))
      (evalo `(list (,f 1)) '(3)))
    '((lambda (x) 3)))

(time-test "b"
    (run 1 (f)
      (fresh (e)
        (== `(lambda (x) ,e) f))
      (evalo `(list (,f 1) (,f 2) (,f 3)) '(3 6 9)))
    '((lambda (x) (match x (1 3) (2 6) (3 9) . _.0))))

(time-test "c"
    (run 1 (f)
      (fresh (e)
        (== `(lambda (x) ,e) f)
        (absento 'match e))
      (evalo `(list (,f 1) (,f 2) (,f 3)) '(1 2 3)))
    '((lambda (x) x)))

(time-test "d"
    (run 1 (f)
      (fresh (e)
        (== `(lambda (x) ,e) f)
        (absento 'match e))
      (evalo `(list (,f 1) (,f 2) (,f 3)) '(2 3 4)))
    '((lambda (x) (+ 1 x))))

(time-test "e"
    (run 1 (f)
      (fresh (e)
        (== `(lambda (x) ,e) f)
        (absento 'match e))
      (evalo `(list (,f 1) (,f 2) (,f 3)) '(3 4 5)))
    '((lambda (x) (+ 2 x))))

(time-test "f"
    (run 1 (f)
      (fresh (e)
        (== `(lambda (x) ,e) f)
        (absento 'match e))
      (evalo `(list (,f 1) (,f 2) (,f 3)) '(173 174 175)))
    '((lambda (x) (+ 172 x))))

(time-test "lool"
    (run 4 (f)
      (evalo `(list (,f 1) (,f 2) (,f 3)) '(1 2 3)))
    '(quote and or ((lambda (_.0) _.0) (sym _.0))))

(printf "this test takes a minute\n")
(time-test "g"
    (run 1 (f)
      (fresh (e)
        (== `(lambda (x) ,e) f)
        (absento 'match e))
      (evalo `(list (,f 0) (,f 1) (,f 2)) '(0 2 4)))
    '((lambda (x) (+ x x))))

(printf "this test takes several minutes\n")
(time-test "h-a"
    (run 1 (f)
      (fresh (e)
        (== `(lambda (x) ,e) f)
        (absento 'match e))
      (evalo `(list (,f 1) (,f 2) (,f 3)) '(3 6 9)))
    '((lambda (x) (* 3 x))))

(printf "this test takes several minutes\n")
(time-test "h-b"
    (run 1 (f)
      (fresh (e)
        (== `(lambda (x) ,e) f)
        (absento 'match e))
      (evalo `(list (,f 1) (,f 2) (,f 3)) '(137 274 411)))
    '((lambda (x) (* 137 x))))

(printf "this test takes several minutes\n")
(time-test "h-c"
    (run 1 (f)
      (fresh (e)
        (== `(lambda (x) ,e) f)
        (absento 'match e))
      (evalo `(list (,f 2) (,f 3) (,f 4)) '(274 411 548)))
    '((lambda (x) (* 137 x))))

(printf "this test takes several minutes\n")
(time-test "i"
    (run 1 (f)
      (fresh (e)
        (== `(lambda (x) ,e) f)
        (absento 'match e))
      (evalo `(list (,f 0) (,f 1) (,f 2)) '(0 3 6)))
    '((lambda (x) (* 3 x))))


#!eof

;;; old tests:

(test "evalo-1"
  (run* (q)
    (evalo '(+ 1 2) q))
  '(3))

(test "evalo-backwards-1"
  (run* (q)
    (evalo `(+ 0 ',q) 3))
  '(3))

(test "evalo-bop-1"
  (run* (q)
    (evalo `((lambda (n) (< n 0)) 0) q))
  '(#f))

(test "evalo-2"
  (run* (q)
    (evalo `(((lambda (f)
                (lambda (n) (if (< n 0) #f
                           (if (= n 0) 1
                               (* n (f (- n 1)))))))
              (lambda (x) 1))
             2)
           q))
  '(2))


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

;; slowish
(test "evalo-fac-9"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac 9))
           q))
  '(362880))

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


;; slowish
(test "evalo-backwards-fac-9"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ,q))
           362880))
  '(9))

;; remember the quote!
(test "evalo-backwards-fac-quoted-9"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ',q))
           362880))
  '(9))


;; slowish
(test "evalo-fac-table"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           q))
  '((1 1 2 6)))

(test "evalo-fac-synthesis-hole-0"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) ',q
                                (* n (fac (- n 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           '(1 1 2 6)))
  '(1))

(test "evalo-fac-synthesis-hole-1"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (,q (- n 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           '(1 1 2 6)))
  '(fac))

;; takes a while
(test "evalo-fac-synthesis-hole-1-reversed-examples"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (,q (- n 1))))))))
              (list
               (fac 3)
               (fac 2)
               (fac 1)
               (fac 0)))
           '(6 2 1 1)))
  '(fac))

(test "evalo-fac-synthesis-hole-2"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- ,q 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           '(1 1 2 6)))
  '(n))

(test "evalo-fac-synthesis-hole-3"
  (run 1 (q)
    (fresh (r s)
      (== (list r s) q)
      (evalo `(letrec ((fac
                        (lambda (n)
                          (if (< n 0) #f
                              (if (= n 0) 1
                                  (* n (fac (- ,r ,s))))))))
                (list
                 (fac 0)
                 (fac 1)
                 (fac 2)
                 (fac 3)))
             '(1 1 2 6))))
  '((n 1)))

;; slow, even with the 'symbolo' constraint on 'q'
(test "evalo-fac-synthesis-hole-4"
  (run 1 (q)
    (symbolo q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (,q n 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           '(1 1 2 6)))
  '(-))


(test "evalo-division-using-multiplication-0"
  (run* (q)
    (evalo `(* 3 ',q) 6))
  '(2))

(test "evalo-division-using-multiplication-1"
  (run* (q)
    (evalo `(* 4 ',q) 6))
  '())

(test "evalo-division-using-multiplication-2"
  (run* (q)
    (evalo `(* 3 ',q) 18))
  '(6))

(test "evalo-many-0"
  (run* (q)
    (fresh (x y)
      (evalo `(* ',x ',y) 6)
      (== q (list x y))))
  '((6 1) (1 6) (-1 -6) (-2 -3)
    (-3 -2) (-6 -1) (2 3) (3 2)))

(test "many-1"
  (run* (q)
    (fresh (x y)
      (evalo `(+ (* ',x ',y) (* ',x ',y)) 6)
      (== q (list x y))))
  '((3 1) (1 3) (-1 -3) (-3 -1)))

(test "many-2"
  (run* (q)
    (fresh (x y)
      (evalo `(* (* ',x ',y) 2) 6)
      (== q (list x y))))
  '((3 1) (1 3) (-1 -3) (-3 -1)))
