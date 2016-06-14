;;; 14 June 02016
;;;
;;; relational parser for 'miniScheme' with 'match'

;;; Important!
;;;
;;; Chez Scheme is happy to accept 'lambda' bodies containing unbound
;;; variables, so long as the variables aren't referenced:
;;;
;;; > (begin (define append (lambda (x) con)) (list list))
;;; (#<procedure list>)
;;;
;;; Basically, the bodies of 'lambdas' need to be grammatically
;;; correct, but should be allowed to contain references to unbound
;;; variables.
;;;
;;; In contrast, this grammatically incorrect definition causes an
;;; error in Chez:
;;;
;;; > (begin (define append (lambda (x) lambda)) (list list))
;;;
;;; Exception: invalid syntax lambda
;;; Type (debug) to enter the debugger.
;;;
;;; whereas this version is legal in Chez, since 'lambda' is shadowed:
;;;
;;; > (begin (define append (lambda (x) (lambda (lambda) lambda))) (list list))
;;; (#<procedure list>)
;;;
;;; The parser should handle these cases correctly.
;;;
;;; Parser should be applid to the examples + definition, in addition
;;; to the definition by itself.



;;; should be able to avoid duplicating the definitions at the bottom
;;; of the file by just adding the parsing code to the evaluator code
;;; in Barliman.

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




;;; should avoid duplicating these definitions

(define (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

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

(define (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(define (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

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

(define empty-env '())

(define initial-env `((list . (val . (closure (lambda x x) ,empty-env)))
                      (not . (val . (closure prim . not)))
                      (equal? . (val . (closure prim . equal?)))
                      (symbol? . (val . (closure prim . symbol?)))
                      (cons . (val . (closure prim . cons)))
                      (null? . (val . (closure prim . null?)))
                      (car . (val . (closure prim . car)))
                      (cdr . (val . (closure prim . cdr)))
                      . ,empty-env))
