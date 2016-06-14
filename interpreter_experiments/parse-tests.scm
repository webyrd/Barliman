(load "chez-load-parse.scm")
(load "mk/test-check.scm")


(test "parse-1"
  (run 1 (q) (parseo '(begin (define append (lambda (x) cons)) (list list))))
  '(_.0))

(test "parse-2"
  (run 1 (q) (parseo '(begin (define append (lambda (x) con)) (list list))))
  '(_.0))

(test "parse-3"
  (run 1 (q) (parseo '(begin (define append (lambda (x) lambda)) (list list))))
  '())

(test "parse-4"
  (run 1 (q) (parseo '(begin (define append (lambda (x) (lambda (lambda) lambda))) (list list))))
  '(_.0))

(test "parse-5"
  (run 1 (q) (parseo '(begin (define append (lambda (x) x)) (append 5))))
  '(_.0))

(test "parse-6"
  (run 1 (q) (parseo '(begin (define append (lambda (x) x)) (appen 5))))
  '(_.0))

(test "parse-7"
  (run 1 (q) (parseo '(begin (define append (lambda x x)) (append 5))))
  '(_.0))

(test "parse-8"
  (run 1 (q) (parseo '(begin (define append (lambda x)) (append 5))))
  '())

(test "parse-9"
  (run 1 (q) (parseo '(begin (defyn append (lambda (x) x)) (append 5))))
  '())

(test "parse-10"
  (run 1 (q) (parseo '(begin (define append (lambda (x) x)))))
  '())

(test "parse-11"
  (run 1 (q) (parseo '(begin (define append (lambda (x) cons)) (list list))))
  '(_.0))

(test "parse-12"
  (run 1 (q) (parseo '(begin (define append (lambda (x) con)) (list list))))
  '(_.0))

(test "parse-13"
  (run 1 (q) (parseo '(begin
                        (define append
                          (lambda (l s)
                            (if (null? l)
                                s
                                (cons (car l)
                                      (append (cdr l) s)))))
                        (list (append '() '())
                              (append '(foo) '(bar))
                              (append '(a b c) '(d e))))))
  '(_.0))

(test "parse-14"
  (run 1 (q) (parseo '(begin
                        (define append
                          (lambda (l s)
                            (if (null? l)
                                
                                (cons (car l)
                                      (append (cdr l) s)))))
                        (list (append '() '())
                              (append '(foo) '(bar))
                              (append '(a b c) '(d e))))))
  '())

(test "parse-15"
  (run 1 (q) (parseo '(begin
                        (define append
                          (lambda 
                            (if (null? l)
                                s
                                (cons (car l)
                                      (append (cdr l) s)))))
                        (list (append '() '())
                              (append '(foo) '(bar))
                              (append '(a b c) '(d e))))))
  '())

(test "parse-16"
  (run 1 (q) (parseo '(begin
                        (define                          
                          (lambda (l s)
                            (if (null? l)
                                s
                                (cons (car l)
                                      (append (cdr l) s)))))
                        (list (append '() '())
                              (append '(foo) '(bar))
                              (append '(a b c) '(d e))))))
  '())

(test "parse-17"
  (run 1 (q) (parseo '(begin
                        (define foo
                          (lambda (x)
                            bar))
                        5)))
  '(_.0))

(test "parse-18"
  (run 1 (q) (parseo '(begin
                        (define foo
                          (lambda (x)
                            and))
                        5)))
  '())

(test "parse-19"
  (run 1 (q) (parseo '(begin
                        (define foo
                          (lambda (x)
                            or))
                        5)))
  '())

(test "parse-20"
  (run 1 (q) (parseo '(begin
                        (define foo
                          (lambda (x)
                            if))
                        5)))
  '())

(test "parse-21"
  (run 1 (q) (parseo '(begin
                        (define foo
                          (lambda (x)
                            begin))
                        5)))
  '())

(test "parse-22"
  (run 1 (q) (parseo '(begin
                        (define foo
                          (lambda (x)
                            define))
                        5)))
  '())

(test "parse-23"
  (run 1 (q) (parseo '(begin
                        (define foo
                          (lambda (x)
                            letrec))
                        5)))
  '())


;;; should be able to check the definition is legal with queries like:

(test "parse-defn-no-tests-1"
  (run 1 (q) (parseo `(begin (define append (lambda (x) cons)) . ,q)))
  '(((_.0) (num _.0))))

(test "parse-defn-no-tests-2"
  (run 1 (q) (parseo `(begin (define append (lambda (x) con)) . ,q)))
  '(((_.0) (num _.0))))

(test "parse-defn-no-tests-3"
  (run 1 (q) (parseo `(begin (define append (lambda cons)) . ,q)))
  '())

(test "parse-defn-no-tests-4"
  (run 1 (q) (parseo `(begin (define append (lambda (x) (if))) . ,q)))
  '())

(test "parse-defn-no-tests-5"
  (run 1 (q) (parseo `(begin (define append (lambda (x) (if 3 4 5))) . ,q)))
  '(((_.0) (num _.0))))

(test "parse-defn-no-tests-6"
  (run 1 (q) (parseo `(begin (define append (lambda (x) (if 3))) . ,q)))
  '())



(test "proof?-1"
  (run 1 (q)
    (fresh (prf)
      (fresh (rule assms ants)
        ;; We want to prove that C holds...
        (== `(,rule ,assms ,ants C) prf)
        ;; ...given the assumptions A, A => B, and B => C.
        (== `(A (if A B) (if B C)) assms)
        (parseo `(letrec ((member? (lambda (x ls)
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
                     (proof? ',prf)))))))
  '(_.0))

(test "proof?-2"
  (run 1 (q)
    (fresh (prf)
      (fresh (rule assms ants)
        ;; We want to prove that C holds...
        (== `(,rule ,assms ,ants C) prf)
        ;; ...given the assumptions A, A => B, and B => C.
        (== `(A (if A B) (if B C)) assms)
        (parseo `(letrec ((member? (lambda (x ls)
                                     (if (null? ls)
                                         #f
                                         (if (equal? (car ls) x)
                                             #t
                                             (member? x (cdr ls)))))))
                   (letrec ((proof? (lambda (proof)
                                      (match ; removed expression to match against
                                        [`(assumption ,assms () ,A)
                                         (member? A assms)]
                                        [`(modus-ponens
                                           ,assms
                                           ((,r1 ,assms ,ants1 (if ,A ,B))
                                            (,r2 ,assms ,ants2 ,A))
                                           ,B)
                                         (and (proof? (list r1 assms ants1 (list 'if A B)))
                                              (proof? (list r2 assms ants2 A)))]))))
                     (proof? ',prf)))))))
  '())

(test "proof?-3"
  (run 1 (q)
    (fresh (prf)
      (fresh (rule assms ants)
        ;; We want to prove that C holds...
        (== `(,rule ,assms ,ants C) prf)
        ;; ...given the assumptions A, A => B, and B => C.
        (== `(A (if A B) (if B C)) assms)
        (parseo `(letrec ((member? (lambda (x ls)
                                     (if (null? ls)
                                         #f
                                         (if (equal? (car ls) x)
                                             #t
                                             (member? x (cdr ls)))))))
                   (letrec ((proof? (lambda (proof)
                                      (match proof
                                        [`(assumption ,assms () ,A)
                                         ;; incorrect use of lambda
                                         (lambda)]
                                        [`(modus-ponens
                                           ,assms
                                           ((,r1 ,assms ,ants1 (if ,A ,B))
                                            (,r2 ,assms ,ants2 ,A))
                                           ,B)
                                         (and (proof? (list r1 assms ants1 (list 'if A B)))
                                              (proof? (list r2 assms ants2 A)))]))))
                     (proof? ',prf)))))))
  '())

(test "proof?-4"
  (run 1 (q)
    (fresh (prf)
      (fresh (rule assms ants)
        ;; We want to prove that C holds...
        (== `(,rule ,assms ,ants C) prf)
        ;; ...given the assumptions A, A => B, and B => C.
        (== `(A (if A B) (if B C)) assms)
        (parseo `(letrec ((member? (lambda (x ls)
                                     (if (null? ls)
                                         #f
                                         (if (equal? (car ls) x)
                                             #t
                                             (member? x (cdr ls)))))))
                   (letrec ((proof? (lambda (proof)
                                      (match proof
                                        [`(assumption ,lambda () ,A)
                                         ;; legal shadowing of lambda
                                         (lambda)]
                                        [`(modus-ponens
                                           ,assms
                                           ((,r1 ,assms ,ants1 (if ,A ,B))
                                            (,r2 ,assms ,ants2 ,A))
                                           ,B)
                                         (and (proof? (list r1 assms ants1 (list 'if A B)))
                                              (proof? (list r2 assms ants2 A)))]))))
                     (proof? ',prf)))))))
  '(_.0))

(test "check-quine-1"
  (run 1 (q)
    (fresh (quine)
      (== '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))) quine)
      (parseo `(letrec ((eval-quasi (lambda (q eval)
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
                   (eval-expr ',quine
                              'initial-env))))))
  '(_.0))

(test "check-quine-2"
  (run 1 (q)
    (fresh (quine)
      (== '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))) quine)
      (parseo `(letrec ((eval-quasi (lambda (q eval)
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
                               [(? symbol? x)
                                ;; illegal match
                                (match x)]
                               [`(quasiquote ,datum)
                                (eval-quasi datum (lambda (exp) (eval-expr exp env)))]
                               [`(,rator ,rand)
                                ((eval-expr rator env) (eval-expr rand env))]
                               ))))
                   (eval-expr ',quine
                              'initial-env))))))
  '())

(test "check-quine-3"
  (run 1 (q)
    (fresh (quine)
      (== '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))) quine)
      (parseo `(letrec ((eval-quasi (lambda (q eval)
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
                               [(? foo? x)
                                ;; illegal predicate name
                                (env x)]
                               [`(quasiquote ,datum)
                                (eval-quasi datum (lambda (exp) (eval-expr exp env)))]
                               [`(,rator ,rand)
                                ((eval-expr rator env) (eval-expr rand env))]
                               ))))
                   (eval-expr ',quine
                              'initial-env))))))
  '())
