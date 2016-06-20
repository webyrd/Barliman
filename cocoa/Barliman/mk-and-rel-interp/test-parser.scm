(load "chez-load-interp.scm")
(load "mk/test-check.scm")

(test 'keyword-reference-1
  (run* (q) (parseo 'quote))
  '())

(test 'letrec-keyword-reference-1
  (run* (q) (parseo '(letrec ((quote (lambda x 5))) quote)))
  '(_.0))

(test 'letrec-keyword-reference-2
  (run* (q) (parseo '(letrec ((foo (lambda x 5))) quote)))
  '())

(test 'letrec-keyword-reference-3
  (run* (q) (parseo '(letrec ((quote (lambda x quote))) 6)))
  '(_.0))

(test 'letrec-keyword-reference-4
  (run* (q) (parseo '(letrec ((bar (lambda (y) quote)) (quote (lambda x x))) 6)))
  '(_.0))

(test 'letrec-keyword-reference-5
  (run* (q) (parseo '(letrec ((quote (lambda x x)) (bar (lambda (y) quote))) 6)))
  '(_.0))

(test 'letrec-keyword-reference-6
  (run* (q) (parseo '(letrec ((foo (lambda x x)) (bar (lambda (y) quote))) 6)))
  '())

(test 'letrec-keyword-reference-7
  (run* (q) (parseo '(letrec ((foo (lambda x x)) (bar (lambda (y) quote))) (bar 6))))
  '())




(test 'begin-keyword-reference-1
  (run* (q) (parseo '(begin (define quote (lambda x 5)) quote)))
  '(_.0))

(test 'begin-keyword-reference-2
  (run* (q) (parseo '(begin (define foo (lambda x 5)) quote)))
  '())

(test 'begin-keyword-reference-3
  (run* (q) (parseo '(begin (define quote (lambda x quote)) 6)))
  '(_.0))

(test 'begin-keyword-reference-4
  (run* (q) (parseo '(begin (define bar (lambda (y) quote)) (define quote (lambda x x)) 6)))
  '(_.0))

(test 'begin-keyword-reference-5
  (run* (q) (parseo '(begin (define quote (lambda x x)) (define bar (lambda (y) quote)) 6)))
  '(_.0))

(test 'begin-keyword-reference-6
  (run* (q) (parseo '(begin (define foo (lambda x x)) (define bar (lambda (y) quote)) 6)))
  '())

(test 'begin-keyword-reference-7
  (run* (q) (parseo '(begin (define foo (lambda x x)) (define bar (lambda (y) quote)) (bar 6))))
  '())




(test 'quote-1
    (run* (q) (parseo '(quote 3)))
'(_.0))

(test 'variable-reference-1
  (run* (q) (parseo 'foo))
  '(_.0))

(test 'function-call-1
    (run* (q) (parseo '(foo 3)))
'(_.0))

(test 'letrec-with-no-bindings-1
  (run* (q)
    (parseo '(letrec ()
               5)))
  '(_.0))

(test 'begin-with-definitions-1
  (run* (q)
    (parseo '(begin 5)))
  '(_.0))

(test 'simple-begin-1
  (run* (q)
    (parseo '(begin
              (define member? (lambda x 6))
              5)))
  '(_.0))

(test 'simple-letrec-1
  (run* (q)
    (parseo '(letrec ((member? (lambda x 6)))
               5)))
  '(_.0))

(test 'parse-append-1
  (run* (q)
    (parseo '(letrec ((append (lambda (l s)
                               (if (null? l)
                                   s
                                   (cons (car l)
                                         (append (cdr l) s))))))
              (append '(1 2 3) '(4 5)))))
  '(_.0))

(test 'parse-even?/odd?-1
  (run* (q)
    (parseo '(letrec ((even? (lambda (n)
                              (if (equal? 'z n)
                                  #t
                                  (odd? (cdr n)))))
                     (odd? (lambda (n)
                             (if (equal? 'z n)
                                 #f
                                 (even? (cdr n))))))
              (even? 'z))))
  '(_.0))

(test 'parse-even?/odd?-2
  (run* (q)
    (parseo '(letrec ((even? (lambda (n)
                               (if (equal? 'z n)
                                   #t
                                   (odd? (cdr n)))))
                      (odd? (lambda (n)
                              (if (equal? 'z n)
                                  #f
                                  (even? (cdr n))))))
               (odd? 'z))))
  '(_.0))

(test 'parse-even?/odd?-3
  (run* (q)
    (parseo '(letrec ((even? (lambda (n)
                               (if (equal? 'z n)
                                   #t
                                   (odd? (cdr n)))))
                      (odd? (lambda (n)
                              (if (equal? 'z n)
                                  #f
                                  (even? (cdr n))))))
               (even? '(s . z)))))
  '(_.0))

(test 'parse-even?/odd?-4
  (run* (q)
    (parseo '(letrec ((even? (lambda (n)
                               (if (equal? 'z n)
                                   #t
                                   (odd? (cdr n)))))
                      (odd? (lambda (n)
                              (if (equal? 'z n)
                                  #f
                                  (even? (cdr n))))))
               (even? '(s . (s . z))))))
  '(_.0))

(test 'parse-even?/odd?-5
  (run* (q)
    (parseo '(letrec ((even? (lambda (n)
                               (if (equal? 'z n)
                                   #t
                                   (odd? (cdr n)))))
                      (odd? (lambda (n)
                              (if (equal? 'z n)
                                  #f
                                  (even? (cdr n))))))
               (odd? '(s . (s . z))))))
  '(_.0))

(test 'parseo-proof-letrec-1
  (run* (prf)
    (fresh (rule assms ants)
      ;; We want to prove that C holds...
      (== `(,rule ,assms ,ants C) prf)
      ;; ...given the assumptions A, A => B, and B => C.
      (== `(A (if A B) (if B C)) assms)
      (parseo
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
            (proof? ',prf))))))
  '(((_.0 (A (if A B) (if B C)) _.1 C) (absento (closure _.0) (closure _.1) (prim _.0) (prim _.1)))))

(test 'proof-call-1
  (run* (q)
    (parseo '(proof? '(modus-ponens
                       (A (if A B) (if B C))
                       ((assumption (A (if A B) (if B C)) () (if B C))
                        (modus-ponens
                         (A (if A B) (if B C))
                         ((assumption (A (if A B) (if B C)) () (if A B))
                          (assumption (A (if A B) (if B C)) () A)) B))
                       C))))
  '(_.0))

(test ''proof-call-in-begin-1
  (run* (q)
    (parseo '(begin
               (proof? '(modus-ponens
                         (A (if A B) (if B C))
                         ((assumption (A (if A B) (if B C)) () (if B C))
                          (modus-ponens
                           (A (if A B) (if B C))
                           ((assumption (A (if A B) (if B C)) () (if A B))
                            (assumption (A (if A B) (if B C)) () A)) B))
                         C)))))
  '(_.0))

(test 'parse-proof-begin-1
  (run* (q)
    (parseo '(begin
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
                         C)))))
  '(_.0))

(test 'parse-proof-begin-2
  (run* (prf)
    (fresh (rule assms ants)
      ;; We want to prove that C holds...
      (== `(,rule ,assms ,ants C) prf)
      ;; ...given the assumptions A, A => B, and B => C.
      (== `(A (if A B) (if B C)) assms)
      (parseo `(begin
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
                 (proof? ',prf)))))
  '(((_.0 (A (if A B) (if B C)) _.1 C) (absento (closure _.0) (closure _.1) (prim _.0) (prim _.1)))))

(test "parse check quine"
  (run* (q)
    (== q '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))
    (parseo
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
                     'initial-env)))))
  '(((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))))

(test "parse generate quine"
  (run* (q)
    (parseo
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
                     'initial-env)))))
  '((_.0 (absento (closure _.0) (prim _.0)))))
