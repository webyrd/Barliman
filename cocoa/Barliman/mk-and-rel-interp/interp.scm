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
            (== `((,x . (val . ,val)) . ,env) env^)
            (parse-expo body env^)))
         ;; Multi-argument
         ((list-of-symbolso x)
          (fresh (vals env^)
            (ext-env*o x vals env env^)
            (parse-expo body env^))))))

    ((fresh (defn args name body e)
       (== `(begin ,defn ,e) expr)
       (== `(define ,name (lambda ,args ,body)) defn)
       (parse-expo `(letrec ((,name (lambda ,args ,body))) ,e) env)))

    ((fresh (rator rands)
       (== `(,rator . ,rands) expr)
       ;; application
       (parse-listo `(,rator . ,rands) env)))

    ((parse-matcho expr env))

    ((fresh (p-name x body letrec-body)
       ;; single-function variadic letrec version
       (== `(letrec ((,p-name (lambda ,x ,body)))
              ,letrec-body)
           expr)
       (not-in-envo 'letrec env)
       (conde
         ; Variadic
         ((symbolo x)
          (fresh (val env^)
            (== `((,x . (val . ,val)) . ,env) env^)
            (parse-expo body env^)))
         ; Multiple argument
         ((list-of-symbolso x)
          (fresh (vals env^)
            (ext-env*o x vals env env^)
            (parse-expo body env^))))
       (parse-expo letrec-body
                     `((,p-name . (rec . (lambda ,x ,body))) . ,env))))

    ((prim-parseo expr env))
    
    ))

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
      ((== `((,var . (val . ,_)) . ,penv) penv-out)
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
  (conde
    ((== `(quote ,val) expr)
     (absento 'closure val)
     (absento 'prim val)
     (not-in-envo 'quote env))

    ((numbero expr) (== expr val))

    ((symbolo expr) (lookupo expr env val))

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
    
    ((fresh (rator x rands body env^ a* res)
       (== `(,rator . ,rands) expr)
       ;; variadic
       (symbolo x)
       (== `((,x . (val . ,a*)) . ,env^) res)
       (eval-expo rator env `(closure (lambda ,x ,body) ,env^))
       (eval-expo body res val)
       (eval-listo rands env a*)))

    ((fresh (rator x* rands body env^ a* res)
       (== `(,rator . ,rands) expr)
       ;; Multi-argument
       (eval-expo rator env `(closure (lambda ,x* ,body) ,env^))
       (eval-listo rands env a*)
       (ext-env*o x* a* env^ res)
       (eval-expo body res val)))

    ((fresh (rator x* rands a* prim-id)
       (== `(,rator . ,rands) expr)
       (eval-expo rator env `(prim . ,prim-id))
       (eval-primo prim-id a* val)
       (eval-listo rands env a*)))
    
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
    
    ((prim-expo expr env val))
    
    ))

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
    [(== prim-id 'cons)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== `(,a . ,d) val))]
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
         ((=/= '() v) (== #f val))))]))

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

(define initial-env `((list . (val . (closure (lambda x x) ,empty-env)))
                      (not . (val . (prim . not)))
                      (equal? . (val . (prim . equal?)))
                      (symbol? . (val . (prim . symbol?)))
                      (cons . (val . (prim . cons)))
                      (null? . (val . (prim . null?)))
                      (car . (val . (prim . car)))
                      (cdr . (val . (prim . cdr)))
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

;; TODO
;;
;; add multi-function letrec
;;
;; make begin support multiple definitions
;;
;; enforce grammar
;;
;; add arithmetic operators using CLP(FD)
