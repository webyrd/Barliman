;; parser

;; see Barliman/interpreter_experiments/parse.scm and
;; Barliman/interpreter_experiments/parse-tests.scm
;; for more on the parser.
(define (parseo expr)
  (parse-expo expr initial-env))

(define (parse-expo expr env)
  (conde
    ((fresh (val)
       (== `(quote ,val) expr)
       (absento 'closure val)
       (absento 'prim val)
       (not-in-envo 'quote env)))

    ((numbero expr))

    ((symbolo expr)
     (fresh (val)
       ;;
       (conde
         ((keywordo expr)
          ;; if expr is a keyword ('lambda', 'begin', 'define', 'letrec', 'and', 'or', 'if', 'match', or whatever)
          ;; *and* expr is shadowed in the environment, succeed.  Otherwuse, it is a syntax violation.
          (fresh (val)
            (lookupo expr env val)))
         ((not-keywordo expr)
          ;; Otherwise, succeed, since it doesn't matter if the symbol
          ;; is bound or not -- the expression is still grammatically
          ;; correct.
          ))))

    ((fresh (x body)
       (== `(lambda ,x ,body) expr)
       (not-in-envo 'lambda env)
       (conde
         ;; Variadic
         ((symbolo x)
          (fresh (val env^)
            (== `((val . (,x . ,val)) . ,env) env^)
            (parse-expo body env^)))
         ;; Multi-argument
         ((list-of-symbolso x)
          (fresh (vals env^)
            (ext-env*o x vals env env^)
            (parse-expo body env^))))))

    ((fresh (defn*/body)
       ;; 'begin' supporting multiple, mutually-recursive definitions
       (== `(begin . ,defn*/body) expr)
       (not-in-envo 'begin env)
       (parse-begino defn*/body env)))

    ((fresh (rator rands)
       (== `(,rator . ,rands) expr)
       ;; application
       (parse-listo `(,rator . ,rands) env)))

    ((parse-matcho expr env))

    ((fresh (binding* letrec-body)
       ;; multiple-function letrec
       (== `(letrec ,binding* ,letrec-body) expr)
       (not-in-envo 'letrec env)
       (parse-letreco binding* letrec-body env)))

    ((prim-parseo expr env))

    ))

(define (parse-begino defn*/body env)
  ;; parse (begin (define name (lambda args e)) ... body)
  ;; as    (letrec ((name (lambda args e)) ...) body)
  (letrec ((parse-begin
            (lambda (defn*/body env letrec-bindings)
              (conde
                ((fresh (body)
                   (== `(,body) defn*/body)
                   (parse-expo `(letrec ,letrec-bindings ,body) env)))
                ((fresh (name args e rest letrec-bindings^)
                   (== `((define ,name (lambda ,args ,e)) . ,rest) defn*/body)
                   (== `((,name (lambda ,args ,e)) . ,letrec-bindings) letrec-bindings^)
                   (parse-begin rest env letrec-bindings^)))))))
    (parse-begin defn*/body env '())))

(define (parse-letreco binding* letrec-body env)
  (letrec ((parse-letreco
            (lambda (binding* letrec-body env env-binding*)
              (conde
                ((== '() binding*)
                 (fresh (env^)
                   (== `((letrec . ,env-binding*) . ,env) env^)
                   (parse-letrec-lambda*o env-binding* env^)
                   (parse-expo letrec-body env^)))
                ((fresh (p-name x body rest)
                   (== `((,p-name (lambda ,x ,body)) . ,rest) binding*)
                   (symbolo p-name)
                   (parse-letreco rest letrec-body env `((rec . (,p-name . (lambda ,x ,body))) . ,env-binding*))))))))
    (parse-letreco binding* letrec-body env '())))

(define (parse-letrec-lambda*o env-binding* env^)
  (conde
    ((== '() env-binding*))
    ((fresh (p-name x body rest)
       (== `((rec . (,p-name . (lambda ,x ,body))) . ,rest) env-binding*)
       (parse-expo `(lambda ,x ,body) env^)
       (parse-letrec-lambda*o rest env^)))))

(define (parse-listo expr env)
  (conde
    ((== '() expr))
    ((fresh (a d)
       (== `(,a . ,d) expr)
       (parse-expo a env)
       (parse-listo d env)))))

(define (prim-parseo expr env)
  (conde
    ((boolean-parseo expr env))
    ((and-parseo expr env))
    ((or-parseo expr env))
    ((if-parseo expr env))))

(define (boolean-parseo expr env)
  (conde
    ((== #t expr))
    ((== #f expr))))

(define (and-parseo expr env)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (not-in-envo 'and env)
    (parse-listo e* env)))

(define (or-parseo expr env)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (not-in-envo 'or env)
    (parse-listo e* env)))

(define (if-parseo expr env)
  (fresh (e1 e2 e3)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (parse-expo e1 env)
    (parse-expo e2 env)
    (parse-expo e3 env)))

(define keywordo
  (lambda (x)
    (conde
      ((== 'quote x))
      ((== 'lambda x))
      ((== 'begin x))
      ((== 'define x))
      ((== 'letrec x))
      ((== 'and x))
      ((== 'or x))
      ((== 'if x))
      ((== 'match x))
      ((== 'quasiquote x))
      ((== 'unquote x)))))

(define not-keywordo
  (lambda (x)
    (fresh ()
      (symbolo x)
      (=/= 'quote x)
      (=/= 'lambda x)
      (=/= 'begin x)
      (=/= 'define x)
      (=/= 'letrec x)
      (=/= 'and x)
      (=/= 'or x)
      (=/= 'if x)
      (=/= 'match x)
      (=/= 'quasiquote x)
      (=/= 'unquote x))))


;;; handle 'match'

(define parse-matcho
  (lambda  (expr env)
    (fresh (against-expr clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (not-in-envo 'match env)
      (parse-expo against-expr env)
      (parse-clauses `(,clause . ,clauses) env))))

(define (parse-clauses clauses env)
  (conde
    ((== '() clauses))
    ((fresh (pat result-expr d)
       (== `((,pat ,result-expr) . ,d) clauses)
       (fresh (penv env^)
         (parse-pattern pat '() penv)
         (regular-env-appendo penv env env^)
         (parse-expo result-expr env^))
       (parse-clauses d env)))))

(define (parse-pattern pat penv penv-out)
  (conde
    ((self-eval-literalo pat)
     (== penv penv-out))
    ((parse-var-pat-match pat penv penv-out))
    ((fresh (var pred)
      (== `(? ,pred ,var) pat)
      (conde
        ((== 'symbol? pred))
        ((== 'number? pred)))
      (parse-var-pat-match var penv penv-out)))
    ((fresh (quasi-pat)
      (== (list 'quasiquote quasi-pat) pat)
      (parse-quasiquote-pattern quasi-pat penv penv-out)))))

(define (parse-var-pat-match var penv penv-out)
  (fresh (_)
    (symbolo var)
    (conde
      ((== penv penv-out)
       (lookupo var penv _))
      ((== `((val . (,var . ,_)) . ,penv) penv-out)
       (not-in-envo var penv)))))

(define (parse-quasiquote-pattern quasi-pat penv penv-out)
  (conde
    ((== penv penv-out)
     (literalo quasi-pat))
    ((fresh (pat)
      (== (list 'unquote pat) quasi-pat)
      (parse-pattern pat penv penv-out)))
    ((fresh (a d penv^)
       (== `(,a . ,d) quasi-pat)
       (=/= 'unquote a)
       (parse-quasiquote-pattern a penv penv^)
       (parse-quasiquote-pattern d penv^ penv-out)))))



;; interpreter
(define (evalo expr val)
  (eval-expo expr initial-env val))

(define (eval-expo expr env val)
  (try-lookupo expr env val (eval-expo-rest expr env val)))
(define (eval-expo-rest expr env val)
  (conde
    ((fresh (rator x* rands a* prim-id)
       (== `(,rator . ,rands) expr)
       (eval-expo rator env `(prim . ,prim-id))
       (eval-primo prim-id a* val)
       (eval-listo rands env a*)))

    ((fresh (rator x rands body env^ a* res)
       (== `(,rator . ,rands) expr)
       ;; variadic
       (symbolo x)
       (== `((val . (,x . ,a*)) . ,env^) res)
       (eval-expo rator env `(closure (lambda ,x ,body) ,env^))
       (eval-expo body res val)
       (eval-listo rands env a*)))

    ((fresh (rator x* rands body env^ a* res)
       (== `(,rator . ,rands) expr)
       ;; Multi-argument
       (eval-expo rator env `(closure (lambda ,x* ,body) ,env^))
       (ext-env*o x* a* env^ res)
       (eval-application rands env a* (eval-expo body res val))))

    ((== `(quote ,val) expr)
     (absento 'closure val)
     (absento 'prim val)
     (not-in-envo 'quote env))

    ((numbero expr) (== expr val))

    ((fresh (x body)
       (== `(lambda ,x ,body) expr)
       (== `(closure (lambda ,x ,body) ,env) val)
       (conde
         ;; Variadic
         ((symbolo x))
         ;; Multi-argument
         ((list-of-symbolso x)))
       (not-in-envo 'lambda env)))

    ((fresh (defn*/body)
       ;; 'begin' supporting multiple, mutually-recursive definitions
       (== `(begin . ,defn*/body) expr)
       (not-in-envo 'begin env)
       (eval-begino defn*/body env val)))

    ((handle-matcho expr env val))

    ((fresh (binding* letrec-body)
       ;; multiple-function variadic letrec
       (== `(letrec ,binding* ,letrec-body) expr)
       (not-in-envo 'letrec env)
       (eval-letreco binding* letrec-body env val)))

    ((prim-expo expr env val))

    ))

(define (eval-begino defn*/body env val)
  (letrec ((eval-begino
            (lambda (defn*/body env val letrec-bindings)
              (conde
                ((fresh (body)
                   (== `(,body) defn*/body)
                   (eval-expo `(letrec ,letrec-bindings ,body) env val)))
                ((fresh (name args e rest)
                   (== `((define ,name (lambda ,args ,e)) . ,rest) defn*/body)
                   (eval-begino rest env val `((,name (lambda ,args ,e)) . ,letrec-bindings))))))))
    (eval-begino defn*/body env val '())))

(define (eval-letreco binding* letrec-body env val)
  (letrec ((eval-letreco
            (lambda (binding* letrec-body env val env-binding*)
              (conde
                ((== '() binding*)
                 (eval-expo letrec-body
                            `((letrec . ,env-binding*) . ,env)
                            val))
                ((fresh (p-name x body rest)
                   (== `((,p-name (lambda ,x ,body)) . ,rest) binding*)
                   (symbolo p-name)
                   (conde
                     ;; Variadic
                     ((symbolo x))
                     ;; Multi-argument
                     ((list-of-symbolso x)))
                   (eval-letreco rest letrec-body env val `((rec . (,p-name . (lambda ,x ,body))) . ,env-binding*))))))))
    (eval-letreco binding* letrec-body env val '())))


(define empty-env '())

(define (lookupo x env v)
  (conde
    ((fresh (y b rest)
       (== `((val . (,y . ,b)) . ,rest) env)
       (conde
         ((== x y) (== b v))
         ((=/= x y) (lookupo x rest v)))))
    ((fresh (env-binding* rest)
       (== `((letrec . ,env-binding*) . ,rest) env)
       (lookup-in-letrec-envo x env-binding* rest env v)))))

(define (lookup-in-letrec-envo x env-binding* rest env v)
  (conde
    ((== '() env-binding*)
     (lookupo x rest v))
    ((fresh (p-name lam-expr env-binding-rest)
       (== `((rec . (,p-name . ,lam-expr)) . ,env-binding-rest) env-binding*)
       (conde
         ((== p-name x)
          (== `(closure ,lam-expr ,env) v))
         ((=/= p-name x)
          (lookup-in-letrec-envo x env-binding-rest rest env v)))))))

; disjunction-passing style, for improving lookupo scheduling
(define (try-lookupo x env v alts)
  (define (try-lookup-in-letrec-envo x env-binding* rest env v alts)
    (conde
      ((== '() env-binding*) (try-lookupo x rest v alts))
      ((fresh (p-name lam-expr env-binding-rest)
         (== `((rec . (,p-name . ,lam-expr)) . ,env-binding-rest) env-binding*)
         (conde
           ((symbolo x) (== p-name x) (== `(closure ,lam-expr ,env) v))
           ((=/= p-name x)
            (try-lookup-in-letrec-envo
              x env-binding-rest rest env v alts)))))))
  (conde
    ((fresh (y b rest)
       (== `((val . (,y . ,b)) . ,rest) env)
       (conde
         ((symbolo x) (== x y) (== b v))
         ((=/= x y) (try-lookupo x rest v alts)))))
    ((fresh (env-binding* rest)
       (== `((letrec . ,env-binding*) . ,rest) env)
       (try-lookup-in-letrec-envo x env-binding* rest env v alts)))
    ((== '() env) alts)))

(define (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((val . (,y . ,b)) . ,rest) env)
       (=/= x y)
       (not-in-envo x rest)))
    ((fresh (env-binding* rest)
       (== `((letrec . ,env-binding*) . ,rest) env)
       (not-in-letrec-envo x env-binding* rest)))))

(define (not-in-letrec-envo x env-binding* rest)
  (conde
    ((== '() env-binding*)
     (not-in-envo x rest))
    ((fresh (p-name lam-expr env-binding-rest)
       (== `((rec . (,p-name . ,lam-expr)) . ,env-binding-rest) env-binding*)
       (=/= p-name x)
       (not-in-letrec-envo x env-binding-rest rest)))))

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
    (let ((tm (walk xs st)))
      (if (pair? tm)
        (loop (cons (car tm) rprefix) (cdr tm))
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
              (let ((rand (walk (car rands) st)))
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
       (== `((val . (,x . ,a)) . ,env) env2)
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
       (== `(,a . ,d) val))]))

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

(define initial-env `((val . (car . (prim . car)))
                      (val . (cdr . (prim . cdr)))
                      (val . (null? . (prim . null?)))
                      (val . (symbol? . (prim . symbol?)))
                      (val . (cons . (prim . cons)))
                      (val . (not . (prim . not)))
                      (val . (equal? . (prim . equal?)))
                      (val . (list . (closure (lambda x x) ,empty-env)))
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
    (=/= 'closure mval)
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

;; TODO
;;
;; add arithmetic operators

;; Barliman helpers -- should stick these somewhere else!
(define (extract-nameso list-of-defns names)
  (conde
    ((== '() list-of-defns)
     (== '() names))
    ((fresh (name ignore rest res)
       (== `((define ,name . ,ignore) . ,rest) list-of-defns)
       (== `(,name . ,res) names)
       (extract-nameso rest res)))))

(define (appendo l s out)
  (conde
    ((== '() l) (== s out))
    ((fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res)))))
