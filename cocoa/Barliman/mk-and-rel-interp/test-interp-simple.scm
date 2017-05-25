(load "mk/mk-vicare.scm")
(load "mk/mk.scm")
(load "mk/test-check.scm")
(load "interp-simple.scm")


(define-syntax test-barliman
  (syntax-rules ()
    ((_ name (qvars ...) lvars program test-case test-result expected-defs)
     (time
       (test name
         (run 1 (defs qvars ...)
           (let ((g1 (gensym "g1"))
                 (g2 (gensym "g2"))
                 (g3 (gensym "g3"))
                 (g4 (gensym "g4"))
                 (g5 (gensym "g5"))
                 (g6 (gensym "g6"))
                 (g7 (gensym "g7"))
                 (g8 (gensym "g8"))
                 (g9 (gensym "g9"))
                 (g10 (gensym "g10"))
                 (g11 (gensym "g11")))
             (fresh lvars
               (absento g1 defs)
               (absento g2 defs)
               (absento g3 defs)
               (absento g4 defs)
               (absento g5 defs)
               (absento g6 defs)
               (absento g7 defs)
               (absento g8 defs)
               (absento g9 defs)
               (absento g10 defs)
               (absento g11 defs)
               (== program defs)
               (evalo (cons 'begin (append program (list test-case)))
                      test-result))))
         expected-defs)))))


(test-barliman 'quasiquote-1 () ()
  '()
  '(list `(1 2) `((cons 3 4) ,(cons 5 6)))
  `((1 2) ((cons 3 4) (5 . 6)))
  `((())))

(test-barliman 'quasiquote-2 (result) ()
  '()
  '(list `(1 2) `((cons 3 4) ,(cons 5 6)) `(,(cons 1 2) `(,(cons 3 4) ,,(cons 5 6))))
  result
  '(((() ((1 2) ((cons 3 4) (5 . 6)) ((1 . 2) `(,(cons 3 4) ,(5 . 6))))))))

(test-barliman 'let*-1 () ()
  '()
  '(let ((x 4))
     (list (let ((x 5) (y (list x x))) y)
           (let* ((x 5) (y (list x x))) y)))
  `((4 4) (5 5))
  `((())))

(test 'cond-1
  (run* (p q)
    (evalo `(cond
              ((null? ',p) 'nil)
              ((not ',p) 'false)
              ((equal? #t ',p) 'true)
              ((number? ',p) 'num)
              ((symbol? ',p) 'sym)
              (else 'must-be-a-pair))
           q))
  '(((() nil))
    ((#f false))
    ((#t true))
    ((_.0 num) (num _.0))
    ((_.0 sym) (sym _.0))
    (((_.0 . _.1) must-be-a-pair))))

(test 'cond-2
  (run* (p q)
    (evalo `(let ((else #f))
              (cond
                ((null? ',p) 'nil)
                ((not ',p) 'false)
                ((equal? #t ',p) 'true)
                ((number? ',p) 'num)
                ((symbol? ',p) 'sym)
                (else 'must-be-a-pair)))
           q))
  '(((() nil))
    ((#f false))
    ((#t true))
    ((_.0 num) (num _.0))
    ((_.0 sym) (sym _.0))
    (((_.0 . _.1) must-be-a-pair))))


(test 'letrec-keyword-reference-1
  (cadaar (run* (q) (evalo '(letrec ((quote (lambda x 5))) quote)
                   q)))
  '(lambda x 5))

(test 'letrec-keyword-reference-2
  (run* (q) (evalo '(letrec ((foo (lambda x 5))) quote)
                   q))
  '())

(test 'letrec-keyword-reference-3
  (run* (q) (evalo '(letrec ((quote (lambda x quote))) 6)
                   q))
  '((6)))

(test 'letrec-keyword-reference-4
  (run* (q) (evalo '(letrec ((bar (lambda (y) quote)) (quote (lambda x x))) 6)
                   q))
  '((6)))

(test 'letrec-keyword-reference-5
  (run* (q) (evalo '(letrec ((quote (lambda x x)) (bar (lambda (y) quote))) 6)
                   q))
  '((6)))

(printf "*** this next test will fail, since 'evalo' doesn't check the syntax of lambda bodies")
(test 'letrec-keyword-reference-6
  (run* (q) (evalo '(letrec ((foo (lambda x x)) (bar (lambda (y) quote))) 6)
                   q))
  '())

(test 'letrec-keyword-reference-7
  (run* (q) (evalo '(letrec ((foo (lambda x x)) (bar (lambda (y) quote))) (bar 6))
                   q))
  '())




(test 'begin-keyword-reference-1
  (cadaar (run* (q) (evalo '(begin (define quote (lambda x 5)) quote)
                   q)))
  '(lambda x 5))

(test 'begin-keyword-reference-2
  (run* (q) (evalo '(begin (define foo (lambda x 5)) quote)
                   q))
  '())

(test 'begin-keyword-reference-3
  (run* (q) (evalo '(begin (define quote (lambda x quote)) 6)
                   q))
  '((6)))

(test 'begin-keyword-reference-4
  (run* (q) (evalo '(begin (define bar (lambda (y) quote)) (define quote (lambda x x)) 6)
                   q))
  '((6)))

(test 'begin-keyword-reference-5
  (run* (q) (evalo '(begin (define quote (lambda x x)) (define bar (lambda (y) quote)) 6)
                   q))
  '((6)))

(printf "*** this next test will fail, since 'evalo' doesn't check the syntax of lambda bodies")
(test 'begin-keyword-reference-6
  (run* (q) (evalo '(begin (define foo (lambda x x)) (define bar (lambda (y) quote)) 6)
                   q))
  '())

(test 'begin-keyword-reference-7
  (run* (q) (evalo '(begin (define foo (lambda x x)) (define bar (lambda (y) quote)) (bar 6))
                   q))
  '())



(test 'letrec-with-no-bindings-1
  (run* (q)
    (evalo '(letrec ()
              5)
           q))
  '((5)))

(test 'begin-with-definitions-1
  (run* (q)
    (evalo '(begin 5) q))
  '((5)))

(test 'simple-begin-1
  (run* (q)
    (evalo '(begin
              (define member? (lambda x 6))
              5)
           q))
  '((5)))

(test 'simple-letrec-1
  (run* (q)
    (evalo '(letrec ((member? (lambda x 6)))
              5)
           q))
  '((5)))

(test 'append-1
  (run* (q)
    (evalo '(letrec ((append (lambda (l s)
                               (if (null? l)
                                   s
                                   (cons (car l)
                                         (append (cdr l) s))))))
              (append '(1 2 3) '(4 5)))
           q))
  '(((1 2 3 4 5))))

(test 'append-2
  (run* (p q)
    (evalo `(letrec ((append (lambda (l s)
                               (if (null? l)
                                   s
                                   (cons (car l)
                                         (append (cdr l) s))))))
              (append ',p ',q))
           '(1 2 3 4 5)))
  '(((() (1 2 3 4 5)))
    (((1) (2 3 4 5)))
    (((1 2) (3 4 5)))
    (((1 2 3) (4 5)))
    (((1 2 3 4) (5)))
    (((1 2 3 4 5) ()))))

(test 'even?/odd?-1
  (run* (q)
    (evalo '(letrec ((even? (lambda (n)
                              (if (equal? 'z n)
                                  #t
                                  (odd? (cdr n)))))
                     (odd? (lambda (n)
                             (if (equal? 'z n)
                                 #f
                                 (even? (cdr n))))))
              (even? 'z))
           q))
  '((#t)))

(test 'even?/odd?-2
  (run* (q) (evalo '(letrec ((even? (lambda (n)
                                      (if (equal? 'z n)
                                          #t
                                          (odd? (cdr n)))))
                             (odd? (lambda (n)
                                     (if (equal? 'z n)
                                         #f
                                         (even? (cdr n))))))
                      (odd? 'z))
                   q))
  '((#f)))

(test 'even?/odd?-3
  (run* (q) (evalo '(letrec ((even? (lambda (n)
                                      (if (equal? 'z n)
                                          #t
                                          (odd? (cdr n)))))
                             (odd? (lambda (n)
                                     (if (equal? 'z n)
                                         #f
                                         (even? (cdr n))))))
                      (even? '(s . z)))
                   q))
  '((#f)))

(test 'even?/odd?-4
  (run* (q) (evalo '(letrec ((even? (lambda (n)
                                      (if (equal? 'z n)
                                          #t
                                          (odd? (cdr n)))))
                             (odd? (lambda (n)
                                     (if (equal? 'z n)
                                         #f
                                         (even? (cdr n))))))
                      (even? '(s . (s . z))))
                   q))
  '((#t)))

(test 'even?/odd?-5
  (run* (q) (evalo '(letrec ((even? (lambda (n)
                                       (if (equal? 'z n)
                                           #t
                                           (odd? (cdr n)))))
                              (odd? (lambda (n)
                                      (if (equal? 'z n)
                                          #f
                                          (even? (cdr n))))))
                       (odd? '(s . (s . z))))
                    q))
  '((#f)))

(test 'proof-letrec-1
  (run 1 (prf)
    (fresh (rule assms ants)
      ;; We want to prove that C holds...
      (== `(,rule ,assms ,ants C) prf)
      ;; ...given the assumptions A, A => B, and B => C.
      (== `(A (if A B) (if B C)) assms)
      (evalo
       `(letrec ((member? (lambda (x ls)
                            (if (null? ls)
                                #f
                                (if (equal? (car ls) x)
                                    #t
                                    (member? x (cdr ls)))))))
          (letrec ((proof? (lambda (proof)
                             (match proof
                               [`(assumption ,assms () ,A)
                                (member? A assms)]
                               [`(modus-ponens
                                  ,assms
                                  ((,r1 ,assms ,ants1 (if ,A ,B))
                                   (,r2 ,assms ,ants2 ,A))
                                  ,B)
                                (and (proof? (list r1 assms ants1 (list 'if A B)))
                                     (proof? (list r2 assms ants2 A)))]))))
            (proof? ',prf)))
       #t)))
  '(((modus-ponens (A (if A B) (if B C))
                  ((assumption (A (if A B) (if B C)) () (if B C))
                   (modus-ponens (A (if A B) (if B C))
                                 ((assumption (A (if A B) (if B C)) () (if A B))
                                  (assumption (A (if A B) (if B C)) () A))
                                 B))
                  C))))


(test 'proof-begin-1
  (run 1 (q)
    (evalo '(begin
              (define member?
                (lambda (x ls)
                  (if (null? ls)
                      #f
                      (if (equal? (car ls) x)
                          #t
                          (member? x (cdr ls))))))
              (define proof?
                (lambda (proof)
                  (match proof
                    [`(assumption ,assms () ,A)
                     (member? A assms)]
                    [`(modus-ponens
                       ,assms
                       ((,r1 ,assms ,ants1 (if ,A ,B))
                        (,r2 ,assms ,ants2 ,A))
                       ,B)
                     (and (proof? (list r1 assms ants1 (list 'if A B)))
                          (proof? (list r2 assms ants2 A)))])))
              (proof? '(modus-ponens
                        (A (if A B) (if B C))
                        ((assumption (A (if A B) (if B C)) () (if B C))
                         (modus-ponens
                          (A (if A B) (if B C))
                          ((assumption (A (if A B) (if B C)) () (if A B))
                           (assumption (A (if A B) (if B C)) () A)) B))
                        C)))
           q))
  '((#t)))

(test 'proof-begin-2
  (run 1 (prf)
    (fresh (rule assms ants)
      ;; We want to prove that C holds...
      (== `(,rule ,assms ,ants C) prf)
      ;; ...given the assumptions A, A => B, and B => C.
      (== `(A (if A B) (if B C)) assms)
      (evalo `(begin
                (define member?
                  (lambda (x ls)
                    (if (null? ls)
                        #f
                        (if (equal? (car ls) x)
                            #t
                            (member? x (cdr ls))))))
                (define proof?
                  (lambda (proof)
                    (match proof
                      [`(assumption ,assms () ,A)
                       (member? A assms)]
                      [`(modus-ponens
                         ,assms
                         ((,r1 ,assms ,ants1 (if ,A ,B))
                          (,r2 ,assms ,ants2 ,A))
                         ,B)
                       (and (proof? (list r1 assms ants1 (list 'if A B)))
                            (proof? (list r2 assms ants2 A)))])))
                (proof? ',prf))
             #t)))
  '(((modus-ponens (A (if A B) (if B C))
                  ((assumption (A (if A B) (if B C)) () (if B C))
                   (modus-ponens (A (if A B) (if B C))
                                 ((assumption (A (if A B) (if B C)) () (if A B))
                                  (assumption (A (if A B) (if B C)) () A))
                                 B))
                  C))))

(test 'begin-append-1
  (run* (q)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                      s
                      (cons (car l)
                            (append (cdr l) s)))))
              (append '(1 2 3) '(4 5)))
           q))
  '(((1 2 3 4 5))))

#|
;;; This results in a cheat.
(time (test 'begin-append-missing-first-recursive-arg-1
        (run 1 (q)
          (evalo `(begin
                    (define append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l)
                                  (append ,q s)))))
                    (append '(1 2 3) '(4 5)))
                 '(1 2 3 4 5)))
        '(((cdr l)))))

;;; This comes up with an ugly, but technically correct answer:
;;; (((match l (`(,_.0 unquote _.1) _.1) . _.2)
;;;   (=/= ((_.0 _.1))) (sym _.0 _.1)))
(time (test 'begin-append-missing-first-recursive-arg-gensym-1
        (run 1 (q)
          (let ((g1 (gensym "g1"))
                (g2 (gensym "g2"))
                (g3 (gensym "g3"))
                (g4 (gensym "g4"))
                (g5 (gensym "g5")))
            (fresh (defn)
              (absento g1 defn)
              (absento g2 defn)
              (absento g3 defn)
              (absento g4 defn)
              (absento g5 defn)
              (== `(define append
                     (lambda (l s)
                       (if (null? l)
                           s
                           (cons (car l)
                                 (append ,q s)))))
                  defn)
              (evalo `(begin
                        ,defn
                        (append '(,g1 ,g2 ,g3) '(,g4 ,g5)))
                     `(,g1 ,g2 ,g3 ,g4 ,g5)))))
        '(((cdr l)))))
|#

(time (test 'begin-append-missing-second-recursive-arg-1
        (run 1 (q)
          (evalo `(begin
                    (define append
                      (lambda (l s)
                        (if (null? l)
                            s
                            (cons (car l)
                                  (append (cdr l) ,q)))))
                    (append '(1 2 3) '(4 5)))
                 '(1 2 3 4 5)))
        '((s))))

(time (test 'begin-append-missing-second-recursive-arg-gensym-1
        (run 1 (q)
          (let ((g1 (gensym "g1"))
                (g2 (gensym "g2"))
                (g3 (gensym "g3"))
                (g4 (gensym "g4"))
                (g5 (gensym "g5")))
            (fresh (defn)
              (absento g1 defn)
              (absento g2 defn)
              (absento g3 defn)
              (absento g4 defn)
              (absento g5 defn)
              (== `(define append
                     (lambda (l s)
                       (if (null? l)
                           s
                           (cons (car l)
                                 (append (cdr l) ,q)))))
                  defn)
              (evalo `(begin
                        ,defn
                        (append '(,g1 ,g2 ,g3) '(,g4 ,g5)))
                     `(,g1 ,g2 ,g3 ,g4 ,g5)))))
        '((s))))

#|
;;; These are too hard for now.
(time (test "append-gensym-synthesis-with-cons-1"
        (run 1 (defn)
          (let ((g1 (gensym "g1"))
                (g2 (gensym "g2"))
                (g3 (gensym "g3"))
                (g4 (gensym "g4"))
                (g5 (gensym "g5"))
                (g6 (gensym "g6"))
                (g7 (gensym "g7")))
            (fresh (a b c d e f g)
              (absento g1 defn)
              (absento g2 defn)
              (absento g3 defn)
              (absento g4 defn)
              (absento g5 defn)
              (absento g6 defn)
              (absento g7 defn)
              (symbolo a)
              (symbolo c)
              (== `(define append
                     (lambda (l s)
                       (if (null? l)
                           s
                           (cons (car l)
                                 (append (,a ,b) ,c)))))
                  defn)
              (evalo `(begin
                        ,defn
                        (cons (append '() '())
                              (cons (append '(,g6) '(,g7))
                                    (cons (append '(,g1 ,g2 ,g3) '(,g4 ,g5))
                                          '()))))
                     `(()
                       (,g6 ,g7)
                       (,g1 ,g2 ,g3 ,g4 ,g5))))))
        '(((define append
             (lambda (l s)
               (if (null? l) s (cons (car l) (append (cdr l) s)))))))))

(time (test "append-gensym-synthesis-with-list-1"
        (run 1 (defn)
          (let ((g1 (gensym "g1"))
                (g2 (gensym "g2"))
                (g3 (gensym "g3"))
                (g4 (gensym "g4"))
                (g5 (gensym "g5"))
                (g6 (gensym "g6"))
                (g7 (gensym "g7")))
            (fresh (a b c d e f g)
              (absento g1 defn)
              (absento g2 defn)
              (absento g3 defn)
              (absento g4 defn)
              (absento g5 defn)
              (absento g6 defn)
              (absento g7 defn)
              (symbolo a)
              (symbolo c)
              (== `(define append
                     (lambda (l s)
                       (if (null? l)
                           s
                           (cons (car l)
                                 (append (,a ,b) ,c)))))
                  defn)
              (evalo `(begin
                        ,defn
                        (list (append '() '())
                              (append '(,g6) '(,g7))
                              (append '(,g1 ,g2 ,g3) '(,g4 ,g5))))
                     `(()
                       (,g6 ,g7)
                       (,g1 ,g2 ,g3 ,g4 ,g5))))))
        '(((define append
            (lambda (l s)
              (if (null? l) s (cons (car l) (append (cdr l) s)))))))))
|#

#|
;;; doesn't come back even after 25 minutes
(time (test 'begin-append-missing-both-recursive-args-gensym-1
        (run 1 (q r)
          (let ((g1 (gensym "g1"))
                (g2 (gensym "g2"))
                (g3 (gensym "g3"))
                (g4 (gensym "g4"))
                (g5 (gensym "g5")))
            (fresh (defn)
              (absento g1 defn)
              (absento g2 defn)
              (absento g3 defn)
              (absento g4 defn)
              (absento g5 defn)
              (== `(define append
                     (lambda (l s)
                       (if (null? l)
                           s
                           (cons (car l)
                                 (append ,q ,r)))))
                  defn)
              (evalo `(begin
                        ,defn
                        (append '(,g1 ,g2 ,g3) '(,g4 ,g5)))
                     `(,g1 ,g2 ,g3 ,g4 ,g5)))))
        '((((cdr l) s)))))
|#

(test "check quine"
  (run 1 (q)
       (== q '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))
       (evalo
         `(letrec ((eval-quasi (lambda (q eval)
                                 (match q
                                   [(? symbol? x) x]
                                   [`() '()]
                                   [`(,`unquote ,exp) (eval exp)]
                                   [`(quasiquote ,datum) ('error)]
                                   [`(,a . ,d)
                                     (cons (eval-quasi a eval) (eval-quasi d eval))]))))
            (letrec ((eval-expr
                       (lambda (expr env)
                         (match expr
                           [`(quote ,datum) datum]
                           [`(lambda (,(? symbol? x)) ,body)
                             (lambda (a)
                               (eval-expr body (lambda (y)
                                                 (if (equal? x y)
                                                   a
                                                   (env y)))))]
                           [(? symbol? x) (env x)]
                           [`(quasiquote ,datum)
                             (eval-quasi datum (lambda (exp) (eval-expr exp env)))]
                           [`(,rator ,rand)
                             ((eval-expr rator env) (eval-expr rand env))]
                           ))))
              (eval-expr ',q
                         'initial-env)))
         q))
  (list (list '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))))


;;; this test used to take ~5 minutes
(printf "*** 'generate quine using Scheme-in-Scheme' test takes ~~12 minutes to run under Chez! ***\n")
(time
  (test "generate quine using Scheme-in-Scheme"
    (run 1 (q)
         (evalo
           `(letrec ((eval-quasi (lambda (q eval)
                                   (match q
                                     [(? symbol? x) x]
                                     [`() '()]
                                     [`(,`unquote ,exp) (eval exp)]
                                     [`(quasiquote ,datum) ('error)]
                                     [`(,a . ,d)
                                       (cons (eval-quasi a eval) (eval-quasi d eval))]))))
              (letrec ((eval-expr
                         (lambda (expr env)
                           (match expr
                             [`(quote ,datum) datum]
                             [`(lambda (,(? symbol? x)) ,body)
                               (lambda (a)
                                 (eval-expr body (lambda (y)
                                                   (if (equal? x y)
                                                     a
                                                     (env y)))))]
                             [(? symbol? x) (env x)]
                             [`(quasiquote ,datum)
                               (eval-quasi datum (lambda (exp) (eval-expr exp env)))]
                             [`(,rator ,rand)
                               ((eval-expr rator env) (eval-expr rand env))]
                             ))))
                (eval-expr ',q
                           'initial-env)))
           q))
    `((((lambda (_.0) `(,_.0 ',_.0)) '(lambda (_.0) `(,_.0 ',_.0))) (=/= ((_.0 ,closure-tag)) ((_.0 ,prim-tag))) (sym _.0)))))


#|
;;; This test used to take 2 minutes.  With pull request #2 (Prioritize symbol lookup)
;;; it no longer comes back after 10 minutes. The other tests are faster, though.
(printf "*** 'generate non-trivial quine old-fashioned way' test takes ~~2 minutes to run under Chez! ***\n")
(time
  (test "generate non-trivial quine old-fashioned way"
    (run 4 (q) (evalo q q))
    `((_.0 (num _.0))
      (#t)
      (#f)
      (((lambda (_.0) (list _.0 (list 'quote _.0)))
        '(lambda (_.0) (list _.0 (list 'quote _.0))))
       (=/= ((_.0 ,closure-tag)) ((_.0 list)) ((_.0 ,prim-tag)) ((_.0 quote))) (sym _.0)))))
|#
