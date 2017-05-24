;; The definition of 'letrec' is based based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

(define closure-tag (gensym "#%closure"))
(define prim-tag (gensym "#%primitive"))

(define (evalo expr val)
  (eval-expo expr initial-env val))

(define (eval-expo expr env val)
  (conde
    ((== `(quote ,val) expr)
     (absento closure-tag val)
     (absento prim-tag val)
     (not-in-envo 'quote env))

    ((numbero expr) (== expr val))

    ((symbolo expr) (lookupo expr env val))

    ((fresh (x body)
       (== `(lambda ,x ,body) expr)
       (== `(,closure-tag (lambda ,x ,body) ,env) val)
       (paramso x)
       (not-in-envo 'lambda env)))

    ((fresh (rator x rands body env^ a* res)
       (== `(,rator . ,rands) expr)
       ;; variadic
       (symbolo x)
       (== `((val . (,x . ,a*)) . ,env^) res)
       (eval-expo rator env `(,closure-tag (lambda ,x ,body) ,env^))
       (eval-expo body res val)
       (eval-listo rands env a*)))

    ((fresh (rator x* rands body env^ a* res)
       (== `(,rator . ,rands) expr)
       ;; Multi-argument
       (eval-expo rator env `(,closure-tag (lambda ,x* ,body) ,env^))
       (eval-listo rands env a*)
       (ext-env*o x* a* env^ res)
       (eval-expo body res val)))

    ((fresh (rator x* rands a* prim-id)
       (== `(,rator . ,rands) expr)
       (eval-expo rator env `(,prim-tag . ,prim-id))
       (eval-primo prim-id a* val)
       (eval-listo rands env a*)))

    ((handle-matcho expr env val))

    ((fresh (begin-body)
       (== `(begin . ,begin-body) expr)
       (eval-begino '() begin-body env val)))

    ((fresh (b* letrec-body)
       (== `(letrec ,b* ,letrec-body) expr)
       (not-in-envo 'letrec env)
       (eval-letreco b* letrec-body env val)))

    ((fresh (b* body)
       (== `(let ,b* ,body) expr)
       (not-in-envo 'let env)
       (let loop ((b* b*) (p* '()) (rand* '()))
         (conde
           ((fresh (a* res)
              (== '() b*)
              (eval-listo rand* env a*)
              (ext-env*o p* a* env res)
              (eval-expo body res val)))
           ((fresh (p rand b*-rest)
              (== `((,p ,rand) . ,b*-rest) b*)
              (symbolo p)
              (loop b*-rest (cons p p*) (cons rand rand*))))))))

    ((fresh (b* body)
       (== `(let* ,b* ,body) expr)
       (not-in-envo 'let env)
       (let loop ((b* b*) (env env))
         (conde
           ((== '() b*) (eval-expo body env val))
           ((fresh (p rand a b*-rest res)
              (== `((,p ,rand) . ,b*-rest) b*)
              (symbolo p)
              (== `((val . (,p . ,a)) . ,env) res)
              (loop b*-rest res)
              (eval-expo rand env a)))))))

    ((fresh (qq-expr)
       (== (list 'quasiquote qq-expr) expr)
       (not-in-envo 'quasiquote env)
       (eval-qq-expo qq-expr env val)))

    ((prim-expo expr env val))))

(define empty-env '())

(define (lookup-reco k renv x b* t)
    (conde
      ((== '() b*) (k))
      ((fresh (b*-rest p-name lam-expr)
         (== `((,p-name . ,lam-expr) . ,b*-rest) b*)
         (conde
           ((== p-name x) (== `(,closure-tag ,lam-expr ,renv) t))
           ((=/= p-name x) (lookup-reco k renv x b*-rest t)))))))
(define (lookupo x env t)
  (conde
    ((fresh (y b rest)
       (== `((val . (,y . ,b)) . ,rest) env)
       (conde
         ((== x y) (== b t))
         ((=/= x y) (lookupo x rest t)))))
    ((fresh (b* rest)
       (== `((rec . ,b*) . ,rest) env)
       (lookup-reco (lambda () (lookupo x rest t)) env x b* t)))))

(define (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((val . (,y . ,b)) . ,rest) env)
       (=/= x y)
       (not-in-envo x rest)))
    ((fresh (b* rest)
       (== `((rec . ,b*) . ,rest) env)
       (not-in-env-reco x b* rest)))))

(define (not-in-env-reco x b* env)
  (conde
    ((== '() b*) (not-in-envo x env))
    ((fresh (p-name lam-expr b*-rest)
       (== `((,p-name . ,lam-expr) . ,b*-rest) b*)
       (=/= p-name x)
       (not-in-env-reco x b*-rest env)))))

(define (eval-letreco b* letrec-body env val)
  (let loop ((b* b*) (rb* '()))
    (conde
      ((== '() b*) (eval-expo letrec-body `((rec . ,rb*) . ,env) val))
      ((fresh (p-name x body b*-rest)
         (== `((,p-name (lambda ,x ,body)) . ,b*-rest) b*)
         (symbolo p-name)
         (paramso x)
         (loop b*-rest `((,p-name . (lambda ,x ,body)) . ,rb*)))))))

;; NOTE: rec-defs is Scheme state, not a logic term!
(define (eval-begino rec-defs begin-body env val)
  (conde
    ((fresh (e)
        (== `(,e) begin-body)
        (if (null? rec-defs)
          (eval-expo e env val)
          (eval-expo `(letrec ,rec-defs ,e) env val))))
    ((fresh (name args body begin-rest)
        (== `((define ,name (lambda ,args ,body)) . ,begin-rest) begin-body)
        (symbolo name)
        (eval-begino
          (cons `(,name (lambda ,args ,body)) rec-defs) begin-rest env val)))))

;; TODO: Match Scheme behavior for multi-level quasiquotes.
(define (eval-qq-expo qq-expr env val)
  (conde
    ((fresh (expr)
       (== (list 'unquote expr) qq-expr)
       (eval-expo expr env val)))
    ((fresh (qq-a qq-d va vd)
       (== `(,qq-a . ,qq-d) qq-expr)
       (== `(,va . ,vd) val)
       (=/= 'unquote qq-a)
       (=/= closure-tag qq-a)
       (=/= prim-tag qq-a)
       (eval-qq-expo qq-a env va)
       (eval-qq-expo qq-d env vd)))
    ((== qq-expr val)
     (conde$
       ((== '() val))
       ((symbolo val))
       ((== #f val))
       ((== #t val))
       ((numbero val))))))

(define (eval-listo expr env val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo a env v-a)
       (eval-listo d env v-d)))))

(define (paramso params)
  (conde
    ; Multiple argument
    ((list-of-paramso params))
    ; Variadic
    ((symbolo params))))

(define (not-in-paramso x params)
  (conde
    ((== '() params))
    ((fresh (a d)
       (== `(,a . ,d) params)
       (=/= a x)
       (not-in-paramso x d)))))

(define (list-of-paramso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-paramso d)
       (not-in-paramso a d)))))

(define (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((val . (,x . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

(define (eval-primo prim-id a* val)
  (conde
    [(== prim-id 'cons)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== `(,a . ,d) val))]
    [(== prim-id 'car)
     (fresh (d)
       (== `((,val . ,d)) a*)
       (=/= closure-tag val)
       (=/= prim-tag val))]
    [(== prim-id 'cdr)
     (fresh (a)
       (== `((,a . ,val)) a*)
       (=/= closure-tag a)
       (=/= prim-tag a))]
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
    [(== prim-id 'pair?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((fresh (a d)
            (== #t val)
            (== `(,a . ,d) v)
            (=/= closure-tag a)
            (=/= prim-tag a)))
         ((== #f val)
          (conde
            ((== '() v))
            ((symbolo v))
            ((== #f v))
            ((== #t v))
            ((numbero v))
            ((fresh (d) (== `(,closure-tag . ,d) v)))
            ((fresh (d) (== `(,prim-tag . ,d) v)))))))]
    [(== prim-id 'procedure?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((== #t val)
          (conde
            ((fresh (d) (== `(,closure-tag . ,d) v)))
            ((fresh (d) (== `(,prim-tag . ,d) v)))))
         ((== #f val)
          (conde
            ((== '() v))
            ((symbolo v))
            ((== #f v))
            ((== #t v))
            ((numbero v))
            ((fresh (a d)
               (== `(,a . ,d) v)
               (=/= closure-tag a)
               (=/= prim-tag a)))))))]))

(define (prim-expo expr env val)
  (conde
    ((boolean-primo expr env val))
    ((and-primo expr env val))
    ((or-primo expr env val))
    ((if-primo expr env val))))

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
      ((=/= #f t) (eval-expo e2 env val))
      ((== #f t) (eval-expo e3 env val)))))

(define initial-env `((val . (list . (,closure-tag (lambda x x) ,empty-env)))
                      (val . (not . (,prim-tag . not)))
                      (val . (equal? . (,prim-tag . equal?)))
                      (val . (symbol? . (,prim-tag . symbol?)))
                      (val . (cons . (,prim-tag . cons)))
                      (val . (null? . (,prim-tag . null?)))
                      (val . (pair? . (,prim-tag . pair?)))
                      (val . (car . (,prim-tag . car)))
                      (val . (cdr . (,prim-tag . cdr)))
                      (val . (procedure? . (,prim-tag . procedure?)))
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
    ((symbolo t) (=/= closure-tag t) (=/= prim-tag t))
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
       (== `((val . (,y . ,v)) . ,rest) env1)
       (== `((val . (,y . ,v)) . ,res) env-out)
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
    (=/= closure-tag mval)
    (=/= prim-tag mval)
    (conde
      ((== mval val)
       (== penv penv-out)
       (lookupo var penv val))
      ((== `((val . (,var . ,mval)) . ,penv) penv-out)
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
       (=/= closure-tag mval)
       (=/= prim-tag mval)
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
