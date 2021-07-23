(load "chez-load-interp.scm")
(load "mk/test-check.scm")

;; Adapted by Kanae Tsushima and Will Byrd from Indiana University's
;; old (~2004) compilers course exercise by R. Kent Dyvig, Daniel
;; P. Friedman, and Oscar Waddell.

;; Here we are trying to use Barliman to synthesize a simple pass that
;; ensures constants (booleans and numbers) are explicitly quoted.

(time
 (test "explicitly-quote-constant synthesize 3 recursions"
   (run 1 (q1 q2 q3)
     (evalo `(begin
               (define Expr
                 (lambda (expr)
                   (match expr
                     [`#f
                      (list 'quote #f)]
                     [`#t
                      (list 'quote #t)]
                     [`,(? number? n)
                      (list 'quote n)]
                     [`,(? symbol? var)
                      var]
                     [`(quote ,datum)
                      (list 'quote datum)]
                     [`(if ,test ,conseq ,altern)
                      (list 'if
                            (Expr test)
                            (Expr conseq)
                            (Expr altern))]
                     [`(lambda (,(? symbol? x)) ,body)
                      ;; (list 'lambda (list x) (Expr body))
                      (list 'lambda (list x) ,q1)
                      ]
                     [`(,rator ,rand)
                      ;; (list (Expr rator) (Expr rand))
                      (list ,q2 ,q3)
                      ])))
               (list (Expr '#t)
                     (Expr '#f)
                     (Expr '5)
                     (Expr '(quote 4))
                     (Expr '(4 5))
                     (Expr '(#t #f))
                     (Expr '(lambda (y) (3 #f)))
                     (Expr '(lambda (z) (z (2 5))))
                     (Expr '(if 5 6 (quote 7)))))
            '('#t
              '#f
              '5
              '4
              ('4 '5)
              ('#t '#f)
              (lambda (y) ('3 '#f))
              (lambda (z) (z ('2 '5)))
              (if '5 '6 '7))))
   '((((Expr body) (Expr rator) (Expr rand))))))


(time
 (test "explicitly-quote-constant synthesize constant cases"
   (run 1 (q)
     (evalo `(begin
               (define Expr
                 (lambda (expr)
                   (match expr
                     [`#f
                      ,q
                      ;; '(quote #f)
                      ]
                     [`#t
                      ;; '(quote #t)
                      ,q
                      ]
                     [`,(? number? n)
                      ,q
                      ;; (list 'quote n)
                      ]
                     [`,(? symbol? var)
                      var]
                     [`(quote ,datum)
                      (list 'quote datum)]
                     [`(if ,test ,conseq ,altern)
                      (list 'if
                            (Expr test)
                            (Expr conseq)
                            (Expr altern))]
                     [`(lambda (,(? symbol? x)) ,body)
                      (list 'lambda (list x) (Expr body))]
                     [`(,rator ,rand)
                      (list (Expr rator) (Expr rand))])))
               (list (Expr '#t)
                     (Expr '#f)
                     (Expr '5)
                     (Expr '(quote 4))
                     (Expr '(4 5))
                     (Expr '(#t #f))
                     (Expr '(lambda (y) (3 #f)))
                     (Expr '(lambda (z) (z (2 5))))
                     (Expr '(if 5 6 (quote 7)))))
            '('#t
              '#f
              '5
              '4
              ('4 '5)
              ('#t '#f)
              (lambda (y) ('3 '#f))
              (lambda (z) (z ('2 '5)))
              (if '5 '6 '7))))
   '(((list 'quote expr)))))

(time
 (test "explicitly-quote-constant run forward"
   (run 1 (q)
     (evalo `(begin
               (define Expr
                 (lambda (expr)
                   (match expr
                     [`#f
                      (list 'quote #f)]
                     [`#t
                      (list 'quote #t)]
                     [`,(? number? n)
                      (list 'quote n)]
                     [`,(? symbol? var)
                      var]
                     [`(quote ,datum)
                      (list 'quote datum)]
                     [`(if ,test ,conseq ,altern)
                      (list 'if
                            (Expr test)
                            (Expr conseq)
                            (Expr altern))]
                     [`(lambda (,(? symbol? x)) ,body)
                      (list 'lambda (list x) (Expr body))]
                     [`(,rator ,rand)
                      (list (Expr rator) (Expr rand))])))
               (list (Expr '#t)
                     (Expr '#f)
                     (Expr '5)
                     (Expr '(quote 4))
                     (Expr '(4 5))
                     (Expr '(#t #f))
                     (Expr '(lambda (y) (3 #f)))
                     (Expr '(lambda (z) (z (2 5))))
                     (Expr '(if 5 6 (quote 7)))))
            q))
   '((('#t
       '#f
       '5
       '4
       ('4 '5)
       ('#t '#f)
       (lambda (y) ('3 '#f))
       (lambda (z) (z ('2 '5)))
       (if '5 '6 '7))))))
