(load "mk/mk-vicare.scm")
(load "mk/mk.scm")
(load "mk/test-check.scm")

;; interpreter
(define (evalo expr val)
  (eval-expo expr initial-env val))

(define (eval-expo expr env val)
  (try-lookup-before expr env val (eval-expo-rest expr env val)))

(define (eval-expo-rest expr env val)
  (conde
    ((numbero expr) (== expr val))

    ((fresh (rator x* rands a* prim-id)
       (== `(,rator . ,rands) expr)
       (eval-expo rator env `(prim . ,prim-id))
       (eval-primo prim-id a* val)
       (eval-listo rands env a*)))

    ((fresh (rator x* rands body env^ a* res)
       (== `(,rator . ,rands) expr)
       ;; Multi-argument
       (eval-expo rator env `(closure (lambda ,x* ,body) ,env^))
       (ext-env*o x* a* env^ res)
       (eval-application rands env a* (eval-expo body res val))))

    ((if-primo expr env val))

    ((fresh (rator x rands body env^ a* res)
       (== `(,rator . ,rands) expr)
       ;; variadic
       (symbolo x)
       (== `((,x . (val . ,a*)) . ,env^) res)
       (eval-expo rator env `(closure (lambda ,x ,body) ,env^))
       (eval-expo body res val)
       (eval-listo rands env a*)))

    ((== `(quote ,val) expr)
     (absento 'closure val)
     (absento 'prim val)
     (not-in-envo 'quote env))

    ((fresh (x body)
       (== `(lambda ,x ,body) expr)
       (== `(closure (lambda ,x ,body) ,env) val)
       (conde
         ;; Variadic
         ((symbolo x))
         ;; Multi-argument
         ((list-of-symbolso x)))
       (not-in-envo 'lambda env)))

    ;; WEB 25 May 2016 -- This rather budget version of 'begin' is
    ;; useful for separating 'define' from the expression 'e',
    ;; specifically for purposes of Barliman.
    ((fresh (defn args name body e)
       (== `(begin ,defn ,e) expr)
       (== `(define ,name (lambda ,args ,body)) defn)
       (eval-expo `(letrec ((,name (lambda ,args ,body))) ,e) env val)))

    ((handle-matcho expr env val))

    ((fresh (p-name x body letrec-body)
       ;; single-function variadic letrec version
       (== `(letrec ((,p-name (lambda ,x ,body)))
              ,letrec-body)
           expr)
       (conde
         ; Variadic
         ((symbolo x))
         ; Multiple argument
         ((list-of-symbolso x)))
       (not-in-envo 'letrec env)
       (eval-expo letrec-body
                  `((,p-name . (rec . (lambda ,x ,body))) . ,env)
                  val)))

    ((prim-expo expr env val))))

(define empty-env '())

(define (lookupo x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (conde
         ((== `(val . ,t) b))
         ((fresh (lam-expr)
            (== `(rec . ,lam-expr) b)
            (== `(closure ,lam-expr ,env) t)))))
      ((=/= x y)
       (lookupo x rest t)))))

(define (try-lookup-before x env t alts)
  (lambdag@ (st)
    (let-values (((rgenv venv) (list-split-ground st env)))
      (let loop ((rgenv rgenv) (alts (conde$ ((symbolo x) (lookupo x venv t))
                                             (alts))))
        (if (null? rgenv) (alts st)
          (let ((rib (car rgenv)))
            (loop (cdr rgenv)
              (fresh (y b)
                (== `(,y . ,b) rib)
                (conde$
                  ((symbolo x) (== x y)
                   (conde$
                     ((== `(val . ,t) b))
                     ((fresh (lam-expr)
                             (== `(rec . ,lam-expr) b)
                             (== `(closure ,lam-expr ,env) t)))))
                  ((=/= x y) alts))))))))))

(define (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))

(define (eval-listo expr env val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
            (== `(,a . ,d) expr)
            (== `(,v-a . ,v-d) val)
            (eval-expo a env v-a)
            (eval-listo d env v-d)))))

(define (list-split-ground st xs)
  (let loop ((rprefix '()) (xs xs))
    (let ((tm (walk xs (state-S st))))
      (if (pair? tm)
        (loop (cons (walk (car tm) (state-S st)) rprefix) (cdr tm))
        (values rprefix xs)))))

(define (eval-application rands aenv a* body-goal)
  (define succeed unit)
  (lambdag@ (st)
    (let-values (((rrands rands-suffix) (list-split-ground st rands)))
      (let-values
        (((ggoals vgoals args-suffix)
          (let loop ((rands (reverse rrands))
                     (ggoals succeed)
                     (vgoals succeed)
                     (args a*))
            (if (null? rands) (values ggoals vgoals args)
              (let ((rand (car rands)))
                (let/vars st (args-rest)
                  (let ((goal (fresh (arg)
                                (== `(,arg . ,args-rest) args)
                                (eval-expo rand aenv arg))))
                    (if (var? rand)
                      (loop (cdr rands) ggoals (fresh () vgoals goal) args-rest)
                      (loop (cdr rands) (fresh () ggoals goal) vgoals args-rest)))))))))
        ((fresh ()
           ggoals    ; try ground arguments first
           body-goal ; then the body
           vgoals    ; then fill in unbound arguments
           ; any unbound final segment of arguments
           (eval-listo rands-suffix aenv args-suffix)) st)))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(define (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(define (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

(define (eval-primo prim-id a* val)
  (conde
    [(== prim-id 'car)
     (fresh (d)
       (== `((,val . ,d)) a*)
       (=/= 'closure val))]
    [(== prim-id 'cdr)
     (fresh (a)
       (== `((,a . ,val)) a*)
       (=/= 'closure a))]
    [(== prim-id 'not)
     (fresh (b)
       (== `(,b) a*)
       (conde
         ((=/= #f b) (== #f val))
         ((== #f b) (== #t val))))]
    [(== prim-id 'equal?)
     (fresh (v1 v2)
       (== `(,v1 ,v2) a*)
       (conde
         ((== v1 v2) (== #t val))
         ((=/= v1 v2) (== #f val))))]
    [(== prim-id 'symbol?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((symbolo v) (== #t val))
         ((numbero v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)))))]
    [(== prim-id 'null?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((== '() v) (== #t val))
         ((=/= '() v) (== #f val))))]
    [(== prim-id 'cons)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== `(,a . ,d) val))]
    ))

(define (prim-expo expr env val)
  (conde
    ((boolean-primo expr env val))
    ((and-primo expr env val))
    ((or-primo expr env val))))

(define (boolean-primo expr env val)
  (conde
    ((== #t expr) (== #t val))
    ((== #f expr) (== #f val))))

(define (and-primo expr env val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (not-in-envo 'and env)
    (ando e* env val)))

(define (ando e* env val)
  (conde
    ((== '() e*) (== #t val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((== #f v)
          (== #f val)
          (eval-expo e1 env v))
         ((=/= #f v)
          (eval-expo e1 env v)
          (ando `(,e2 . ,e-rest) env val)))))))

(define (or-primo expr env val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (not-in-envo 'or env)
    (oro e* env val)))

(define (oro e* env val)
  (conde
    ((== '() e*) (== #f val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((=/= #f v)
          (== v val)
          (eval-expo e1 env v))
         ((== #f v)
          (eval-expo e1 env v)
          (oro `(,e2 . ,e-rest) env val)))))))

(define (if-primo expr env val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (eval-expo e1 env t)
    (conde
      ((== #t t) (eval-expo e2 env val))
      ((== #f t) (eval-expo e3 env val))
      ; Adding this line would restore normal Scheme semantics, but
      ; unfortunately it defeats the performance improvement we just gained.
      ;((=/= #t t) (=/= #f t) (eval-expo e2 env val))
      )))

(define initial-env `((cons . (val . (prim . cons)))
                      (car . (val . (prim . car)))
                      (cdr . (val . (prim . cdr)))
                      (null? . (val . (prim . null?)))
                      (symbol? . (val . (prim . symbol?)))
                      (not . (val . (prim . not)))
                      (equal? . (val . (prim . equal?)))
                      (list . (val . (closure (lambda x x) ,empty-env)))
                      . ,empty-env))

(define handle-matcho
  (lambda  (expr env val)
    (fresh (against-expr mval clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (not-in-envo 'match env)
      (eval-expo against-expr env mval)
      (match-clauses mval `(,clause . ,clauses) env val))))

(define (not-symbolo t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (not-numbero t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (self-eval-literalo t)
  (conde
    ((numbero t))
    ((booleano t))))

(define (literalo t)
  (conde
    ((numbero t))
    ((symbolo t) (=/= 'closure t))
    ((booleano t))
    ((== '() t))))

(define (booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

(define (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

(define (match-clauses mval clauses env val)
  (fresh (p result-expr d penv)
    (== `((,p ,result-expr) . ,d) clauses)
    (conde
      ((fresh (env^)
         (p-match p mval '() penv)
         (regular-env-appendo penv env env^)
         (eval-expo result-expr env^ val)))
      ((p-no-match p mval '() penv)
       (match-clauses mval d env val)))))

(define (var-p-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= 'closure mval)
    (conde
      ((== mval val)
       (== penv penv-out)
       (lookupo var penv val))
      ((== `((,var . (val . ,mval)) . ,penv) penv-out)
       (not-in-envo var penv)))))

(define (var-p-no-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= mval val)
    (== penv penv-out)
    (lookupo var penv val)))

(define (p-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (== p mval)
     (== penv penv-out))
    ((var-p-match p mval penv penv-out))
    ((fresh (var pred val)
      (== `(? ,pred ,var) p)
      (conde
        ((== 'symbol? pred)
         (symbolo mval))
        ((== 'number? pred)
         (numbero mval)))
      (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-match quasi-p mval penv penv-out)))))

(define (p-no-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (=/= p mval)
     (== penv penv-out))
    ((var-p-no-match p mval penv penv-out))
    ((fresh (var pred val)
       (== `(? ,pred ,var) p)
       (== penv penv-out)
       (symbolo var)
       (conde
         ((== 'symbol? pred)
          (conde
            ((not-symbolo mval))
            ((symbolo mval)
             (var-p-no-match var mval penv penv-out))))
         ((== 'number? pred)
          (conde
            ((not-numbero mval))
            ((numbero mval)
             (var-p-no-match var mval penv penv-out)))))))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-no-match quasi-p mval penv penv-out)))))

(define (quasi-p-match quasi-p mval penv penv-out)
  (conde
    ((== quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
      (== (list 'unquote p) quasi-p)
      (p-match p mval penv penv-out)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (== `(,v1 . ,v2) mval)
       (=/= 'unquote a)
       (quasi-p-match a v1 penv penv^)
       (quasi-p-match d v2 penv^ penv-out)))))

(define (quasi-p-no-match quasi-p mval penv penv-out)
  (conde
    ((=/= quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (=/= 'closure mval)
       (p-no-match p mval penv penv-out)))
    ((fresh (a d)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== penv penv-out)
       (literalo mval)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== `(,v1 . ,v2) mval)
       (conde
         ((quasi-p-no-match a v1 penv penv^))
         ((quasi-p-match a v1 penv penv^)
          (quasi-p-no-match d v2 penv^ penv-out)))))))



(time (test 'list-nth-element-peano
  (run 1 (q r)
    (evalo `(begin
              (define nth
                (lambda (n xs)
                  (if (null? n) ,q ,r)))
              (list
                (nth '() '(foo bar))
                (nth '(s) '(foo bar))
                (nth '() '(1 2 3))
                (nth '(s) '(1 2 3))
                (nth '(s s) '(1 2 3))))
           (list 'foo 'bar 1 2 3)))
  '((((car xs) (nth (cdr n) (cdr xs)))))))


(test 'append-empty
  (run 1 (q)
       (evalo
         `(begin
            (define append
              (lambda (l s)
                (if (null? l)
                  s
                  (cons (car l)
                        (append (cdr l) s)))))
            (append '() '()))
         '()))
  '((_.0)))

(test 'append-all-answers
  (run* (l1 l2)
        (evalo `(begin
                  (define append
                    (lambda (l s)
                      (if (null? l)
                        s
                        (cons (car l)
                              (append (cdr l) s)))))
                  (append ',l1 ',l2))
               '(1 2 3 4 5)))
  '(((() (1 2 3 4 5)))
    (((1) (2 3 4 5)))
    (((1 2) (3 4 5)))
    (((1 2 3) (4 5)))
    (((1 2 3 4) (5)))
    (((1 2 3 4 5) ()))))

;;; flipping rand/body eval order makes this one too hard,
;;; but dynamic ordering via eval-application fixes it!
(test 'append-cons-first-arg
  (run 1 (q)
    (evalo `(begin (define append
                     (lambda (l s)
                       (if (null? l)
                         s
                         (cons ,q
                               (append (cdr l) s)))))
                   (append '(1 2 3) '(4 5)))
           '(1 2 3 4 5)))
  '(((car l))))

(test 'append-cdr-arg
  (run 1 (q)
       (evalo `(begin
                 (define append
                   (lambda (l s)
                     (if (null? l)
                       s
                       (cons (car l)
                             (append (cdr ,q) s)))))
                 (append '(1 2 3) '(4 5)))
              '(1 2 3 4 5)))
  '((l)))

(test 'append-cdr
  (run 1 (q)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                    s
                    (cons (car l)
                          (append (,q l) s)))))
              (append '(1 2 3) '(4 5)))
           '(1 2 3 4 5)))
  '((cdr)))

(time (test 'append-hard-1
  (run 1 (q r)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                    s
                    (cons (car l)
                          (append (,q ,r) s)))))
              (append '(1 2 3) '(4 5)))
           '(1 2 3 4 5)))
  '(((cdr l)))))

(time (test 'append-hard-2
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

(time (test 'append-hard-3
  (run 1 (q r)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                    s
                    (cons (car l)
                          (append ,q ,r)))))
              (list
                (append '(foo) '(bar))
                (append '(1 2 3) '(4 5))))
           (list '(foo bar) '(1 2 3 4 5))))
  '((((cdr l) s)))))

(time (test 'append-hard-4
  (run 1 (q)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                    s
                    (cons (car l)
                          (append . ,q)))))
              (list
                (append '(foo) '(bar))
                (append '(1 2 3) '(4 5))))
           (list '(foo bar) '(1 2 3 4 5))))
  '((((cdr l) s)))))

(time (test 'append-hard-5
  (run 1 (q r)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                    s
                    (cons ,q
                          (append . ,r)))))
              (list
                (append '() '())
                (append '(foo) '(bar))
                (append '(1 2 3) '(4 5))))
           (list '() '(foo bar) '(1 2 3 4 5))))
  '((((car l) ((cdr l) s))))))

;; the following are still overfitting
;; probably need to demote quote and some others

(time
 (test 'append-hard-6-gensym-dummy-test
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh ()
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (fresh (q a b)

           (== `(append ,a ,b) q)

           (== `(define append
                  (lambda (l s)
                    (if (null? l)
                        s
                        (cons (car l) ,q))))
               defn)
           (evalo `(begin
                     ,defn
                     (list
                      (append '() '())
                      (append '(,g1) '(,g2))
                      (append '(,g3 ,g4 ,g5) '(,g6 ,g7))))
                  (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6 ,g7)))))))
   '(((define append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))))))

(printf "append-hard-6-gensym-less-dummy-test takes ~~16s\n")
(time
 (test 'append-hard-6-gensym-less-dummy-test
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh ()
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (fresh (q a b c)

           (== `(,a ,b ,c) q)

           (== `(define append
                  (lambda (l s)
                    (if (null? l)
                        s
                        (cons (car l) ,q))))
               defn)
           (evalo `(begin
                     ,defn
                     (list
                      (append '() '())
                      (append '(,g1) '(,g2))
                      (append '(,g3 ,g4 ,g5) '(,g6 ,g7))))
                  (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6 ,g7)))))))
   '(((define append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))))))

(printf "append-hard-6-no-gensym returns an over-specific, incorrect answer\n")
(time (test 'append-hard-6-no-gensym
  (run 1 (q)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                    s
                    (cons (car l) ,q))))
              (list
                (append '() '())
                (append '(foo) '(bar))
                (append '(1 2 3) '(4 5))))
           (list '() '(foo bar) '(1 2 3 4 5))))
  '(((append (cdr l) s)))))

(time
 (test 'append-hard-6-gensym
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh ()
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (fresh (q)
           (== `(define append
                  (lambda (l s)
                    (if (null? l)
                        s
                        (cons (car l) ,q))))
               defn)
           (evalo `(begin
                     ,defn
                     (list
                      (append '() '())
                      (append '(,g1) '(,g2))
                      (append '(,g3 ,g4 ,g5) '(,g6 ,g7))))
                  (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6 ,g7)))))))
   '(((define append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))))))

(time
 (test 'append-hard-7-gensym
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh ()
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (fresh (q r)
           (== `(define append
                  (lambda (l s)
                    (if (null? l)
                        s
                        (cons ,q ,r))))
               defn)
           (evalo `(begin
                     ,defn
                     (list
                      (append '() '())
                      (append '(,g1) '(,g2))
                      (append '(,g3 ,g4 ,g5) '(,g6 ,g7))))
                  (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6 ,g7)))))))
   '(((define append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))))))

(test 'append-hard-7-no-gensym
  (run 1 (q r)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                    s
                    (cons ,q ,r))))
              (list
                (append '() '())
                (append '(foo) '(bar))
                (append '(1 2 3) '(4 5))))
           (list '() '(foo bar) '(1 2 3 4 5))))
  '((((car l) (append (cdr l) s)))))

(time
 (test 'append-hard-8-gensym
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh ()
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (fresh (q)
           (== `(define append
                  (lambda (l s)
                    (if (null? l)
                        s
                        ,q)))
               defn)
           (evalo `(begin
                     ,defn
                     (list
                      (append '() '())
                      (append '(,g1) '(,g2))
                      (append '(,g3 ,g4 ,g5) '(,g6 ,g7))))
                  (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6 ,g7)))))))
   '(((define append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))))))

(time
 (test 'append-hard-9-gensym
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh ()
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (fresh (q r)
           (== `(define append
                  (lambda (l s)
                    (if (null? l)
                        ,q
                        ,r)))
               defn)
           (evalo `(begin
                     ,defn
                     (list
                      (append '() '())
                      (append '(,g1) '(,g2))
                      (append '(,g3 ,g4 ,g5) '(,g6 ,g7))))
                  (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6 ,g7)))))))
   '(((define append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))))))

(time
 (test 'append-hard-10-gensym
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh ()
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (fresh (q r s)
           (== `(define append
                  (lambda (l s)
                    (if (null? ,q)
                        ,r
                        ,s)))
               defn)
           (evalo `(begin
                     ,defn
                     (list
                      (append '() '())
                      (append '(,g1) '(,g2))
                      (append '(,g3 ,g4 ,g5) '(,g6 ,g7))))
                  (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6 ,g7)))))))
   '(((define append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))))))

(time
 (test 'append-hard-11-gensym
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh ()
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (fresh (q r s t)
           (== `(define append
                  (lambda (l s)
                    (if (,t ,q)
                        ,r
                        ,s)))
               defn)
           (evalo `(begin
                     ,defn
                     (list
                      (append '() '())
                      (append '(,g1) '(,g2))
                      (append '(,g3 ,g4 ,g5) '(,g6 ,g7))))
                  (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6 ,g7)))))))
   '(((define append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))))))

(time
 (test 'append-hard-12-gensym
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh ()
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (fresh (q r s)
           (== `(define append
                  (lambda (l s)
                    (if ,q
                        ,r
                        ,s)))
               defn)
           (evalo `(begin
                     ,defn
                     (list
                      (append '() '())
                      (append '(,g1) '(,g2))
                      (append '(,g3 ,g4 ,g5) '(,g6 ,g7))))
                  (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6 ,g7)))))))
   '(((define append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))))))

(time
 (test 'append-hard-13-gensym
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh ()
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (fresh (q)
           (== `(define append
                  (lambda (l s) ,q))
               defn)
           (evalo `(begin
                     ,defn
                     (list
                      (append '() '())
                      (append '(,g1) '(,g2))
                      (append '(,g3 ,g4 ,g5) '(,g6 ,g7))))
                  (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6 ,g7)))))))
   '(((define append (lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))))))

; append-hard-15-gensym seems just as good, so don't waste time on this test case by default
;(time
 ;(test 'append-hard-14-gensym
   ;(run 1 (defn)
     ;(let ((g1 (gensym "g1"))
           ;(g2 (gensym "g2"))
           ;(g3 (gensym "g3"))
           ;(g4 (gensym "g4"))
           ;(g5 (gensym "g5"))
           ;(g6 (gensym "g6"))
           ;(g7 (gensym "g7")))
       ;(fresh ()
         ;(absento g1 defn)
         ;(absento g2 defn)
         ;(absento g3 defn)
         ;(absento g4 defn)
         ;(absento g5 defn)
         ;(absento g6 defn)
         ;(absento g7 defn)
         ;(fresh (p q r)
           ;(== `(define ,p
                  ;(lambda ,q ,r))
               ;defn)
           ;(evalo `(begin
                     ;,defn
                     ;(list
                      ;(append '() '())
                      ;(append '(,g1) '(,g2))
                      ;(append '(,g3 ,g4 ,g5) '(,g6 ,g7))))
                  ;(list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6 ,g7)))))))
   ;'(((define append (lambda (_.0 _.1) (if (null? _.0) _.1 (cons (car _.0) (append (cdr _.0) _.1))))) (sym _.0 _.1)))))

; this one seems just as fast as append-hard-14-gensym
(time
 (test 'append-hard-15-gensym
   (run 1 (defn)
     (let ((g1 (gensym "g1"))
           (g2 (gensym "g2"))
           (g3 (gensym "g3"))
           (g4 (gensym "g4"))
           (g5 (gensym "g5"))
           (g6 (gensym "g6"))
           (g7 (gensym "g7")))
       (fresh ()
         (absento g1 defn)
         (absento g2 defn)
         (absento g3 defn)
         (absento g4 defn)
         (absento g5 defn)
         (absento g6 defn)
         (absento g7 defn)
         (evalo `(begin
                   ,defn
                   (list
                     (append '() '())
                     (append '(,g1) '(,g2))
                     (append '(,g3 ,g4 ,g5) '(,g6 ,g7))))
                (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6 ,g7))))))
   '(((define append
        (lambda (_.0 _.1)
          (if (null? _.0)
            _.1
            (cons (car _.0)
                  (append (cdr _.0) _.1)))))
      (sym _.0 _.1)))))

(time (test 'reverse-hard-1
  (run 1 (q r s)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l) s
                    (cons (car l)
                          (append (cdr l) s)))))
              (begin
                (define reverse
                  (lambda (xs)
                    (if (null? xs) '()
                      (,q (reverse ,r) ,s))))
                (list
                  (reverse '())
                  (reverse '(a))
                  (reverse '(foo bar))
                  (reverse '(1 2 3)))))
          (list '() '(a) '(bar foo) '(3 2 1))))
  '(((append (cdr xs) (list (car xs)))))))

;(time (test 'reverse-hard-2
  ;(run 1 (q r s)
    ;(evalo `(begin
              ;(define append
                ;(lambda (l s)
                  ;(if (null? l) s
                    ;(cons (car l)
                          ;(append (cdr l) s)))))
              ;(begin
                ;(define reverse
                  ;(lambda (xs)
                    ;(if (null? xs) '()
                      ;(append (,q ,r) ,s))))
                ;(list
                  ;(reverse '())
                  ;(reverse '(a))
                  ;(reverse '(foo bar))
                  ;(reverse '(1 2 3)))))
           ;(list '() '(a) '(bar foo) '(3 2 1))))
  ;'(((append (cdr xs) (list (car xs)))))))
