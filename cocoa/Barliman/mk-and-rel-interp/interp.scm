(define empty-env '())

;; parser

;; see Barliman/interpreter_experiments/parse.scm and
;; Barliman/interpreter_experiments/parse-tests.scm
;; for more on the parser.
(define (parseo expr)
  (parse-expo expr parse-initial-env))

(define (parse-expo expr env)
  (conde
    ((fresh (val)
       (== `(quote ,val) expr)
       (absento 'closure val)
       (absento 'prim val)
       (parse-not-in-envo 'quote env)))

    ((numbero expr))

    ((symbolo expr)
     (fresh (val)
       ;;
       (conde
         ((keywordo expr)
          ;; if expr is a keyword ('lambda', 'begin', 'define', 'letrec', 'and', 'or', 'if', 'match', or whatever)
          ;; *and* expr is shadowed in the environment, succeed.  Otherwise, it is a syntax violation.
          (fresh (val)
            (lookupo expr env val)))
         ((not-keywordo expr)
          ;; Otherwise, succeed, since it doesn't matter if the symbol
          ;; is bound or not -- the expression is still grammatically
          ;; correct.
          ))))

    ((fresh (x body)
       (== `(lambda ,x ,body) expr)
       (parse-not-in-envo 'lambda env)
       (conde
         ;; Variadic
         ((symbolo x)
          (fresh (val env^)
            (== `((val . (,x . ,val)) . ,env) env^)
            (parse-expo body env^)))
         ;; Multi-argument
         ((list-of-symbolso x)
          (fresh (vals env^)
            (parse-ext-env*o x vals env env^)
            (parse-expo body env^))))))

    ((fresh (defn*/body)
       ;; 'begin' supporting multiple, mutually-recursive definitions
       (== `(begin . ,defn*/body) expr)
       (parse-not-in-envo 'begin env)
       (parse-begino defn*/body env)))

    ((fresh (rator rands)
       (== `(,rator . ,rands) expr)
       ;; application
       (parse-listo `(,rator . ,rands) env)))

    ((parse-matcho expr env))

    ((fresh (binding* letrec-body)
       ;; multiple-function letrec
       (== `(letrec ,binding* ,letrec-body) expr)
       (parse-not-in-envo 'letrec env)
       (parse-letreco binding* letrec-body env)))

    ((fresh (b* body)
       (== `(let ,b* ,body) expr)
       (parse-not-in-envo 'let env)
       (let loop ((b* b*) (p* '()) (rand* '()))
         (conde
           ((fresh (a* res)
              (== '() b*)
              (parse-ext-env*o p* a* env res)
              (parse-listo rand* env)
              (parse-expo body res)))
           ((fresh (p rand b*-rest)
              (== `((,p ,rand) . ,b*-rest) b*)
              (symbolo p)
              (loop b*-rest (cons p p*) (cons rand rand*))))))))

    ((fresh (b* body)
       (== `(let* ,b* ,body) expr)
       (parse-not-in-envo 'let env)
       (let loop ((b* b*) (env env))
         (conde
           ((== '() b*) (parse-expo body env))
           ((fresh (p rand a b*-rest res)
              (== `((,p ,rand) . ,b*-rest) b*)
              (symbolo p)
              (== `((val . (,p . ,a)) . ,env) res)
              (loop b*-rest res)
              (parse-expo rand env)))))))

    ((fresh (qq-expr)
       (== (list 'quasiquote qq-expr) expr)
       (parse-not-in-envo 'quasiquote env)
       (parse-qq-expo qq-expr env)))

    ((prim-parseo expr env))

    ))

(define (parse-qq-expo qq-expr env)
  (conde
    ((fresh (expr)
       (== (list 'unquote expr) qq-expr)
       (parse-expo expr env)))
    ((fresh (qq-a qq-d)
       (== `(,qq-a . ,qq-d) qq-expr)
       (=/= 'unquote qq-a)
       (=/= 'closure qq-a)
       (=/= 'prim qq-a)
       (parse-qq-expo qq-a env)
       (parse-qq-expo qq-d env)))
    ((conde$
       ((== '() qq-expr))
       ((symbolo qq-expr))
       ((== #f qq-expr))
       ((== #t qq-expr))
       ((numbero qq-expr))))))


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
                   (symbolo name)
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
    ((if-parseo expr env))
    ((cond-parseo expr env))))

(define (boolean-parseo expr env)
  (conde
    ((== #t expr))
    ((== #f expr))))

(define (and-parseo expr env)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (parse-not-in-envo 'and env)
    (parse-listo e* env)))

(define (or-parseo expr env)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (parse-not-in-envo 'or env)
    (parse-listo e* env)))

(define (if-parseo expr env)
  (fresh (e1 e2 e3)
    (== `(if ,e1 ,e2 ,e3) expr)
    (parse-not-in-envo 'if env)
    (parse-expo e1 env)
    (parse-expo e2 env)
    (parse-expo e3 env)))

(define (cond-parseo expr env)
  (fresh (c c*)
    (== `(cond ,c . ,c*) expr)
    (parse-not-in-envo 'cond env)
    (parse-cond-clauses `(,c . ,c*) env)))

(define (parse-cond-clauses c* env)
  (conde
    ((fresh (test conseq)
       (== `((,test ,conseq)) c*)
       (conde
         ((== 'else test))
         ((=/= 'else test)
          (parse-expo test env)))
       (parse-expo conseq env)))
    ((fresh (test conseq c*-rest)
       (== `((,test ,conseq) . ,c*-rest) c*)
       ;; ensure expressions like (cond (else 5) (6 7))
       ;; don't parse, unless 'else' has been shadowed
       (conde
         ((== 'else test)
          (fresh (v) (lookupo test env v)))
         ((=/= 'else test)
          (parse-expo test env)))
       (parse-expo conseq env)
       (parse-cond-clauses c*-rest env)))))

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
      ((== 'conde x))
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
      (=/= 'conde x)
      (=/= 'match x)
      (=/= 'quasiquote x)
      (=/= 'unquote x))))


;;; handle 'match'

(define parse-matcho
  (lambda  (expr env)
    (fresh (against-expr clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (parse-not-in-envo 'match env)
      (parse-expo against-expr env)
      (parse-clauses `(,clause . ,clauses) env))))

(define (parse-clauses clauses env)
  (conde
    ((== '() clauses))
    ((fresh (pat result-expr d)
       (== `((,pat ,result-expr) . ,d) clauses)
       (fresh (penv env^)
         (parse-pattern pat '() penv)
         (parse-regular-env-appendo penv env env^)
         (parse-expo result-expr env^))
       (parse-clauses d env)))))

(define (parse-pattern pat penv penv-out)
  (conde
    ((self-eval-literalo pat)
     (== penv penv-out))
    ((parse-var-pat-match pat penv penv-out))
    ((fresh (datum)
       (== `(quote ,datum) pat)
       (== penv penv-out)))
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
       (parse-lookupo var penv _))
      ((== `((val . (,var . ,_)) . ,penv) penv-out)
       (parse-not-in-envo var penv)))))

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


;;; parse-specific helpers
(define (parse-not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((val . (,y . ,b)) . ,rest) env)
       (=/= x y)
       (parse-not-in-envo x rest)))
    ((fresh (env-binding* rest)
       (== `((letrec . ,env-binding*) . ,rest) env)
       (parse-not-in-letrec-envo x env-binding* rest)))))

(define (parse-not-in-letrec-envo x env-binding* rest)
  (conde
    ((== '() env-binding*)
     (parse-not-in-envo x rest))
    ((fresh (p-name lam-expr env-binding-rest)
       (== `((rec . (,p-name . ,lam-expr)) . ,env-binding-rest) env-binding*)
       (=/= p-name x)
       (parse-not-in-letrec-envo x env-binding-rest rest)))))

(define (parse-ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((val . (,x . ,a)) . ,env) env2)
       (symbolo x)
       (parse-ext-env*o dx* da* env2 out)))))

(define (parse-regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((val . (,y . ,v)) . ,rest) env1)
       (== `((val . (,y . ,v)) . ,res) env-out)
       (parse-regular-env-appendo rest env2 res)))))

(define (parse-lookupo x env v)
  (conde
    ((fresh (y b rest)
       (== `((val . (,y . ,b)) . ,rest) env)
       (conde
         ((== x y) (== b v))
         ((=/= x y) (parse-lookupo x rest v)))))
    ((fresh (env-binding* rest)
       (== `((letrec . ,env-binding*) . ,rest) env)
       (parse-lookup-in-letrec-envo x env-binding* rest env v)))))

(define (parse-lookup-in-letrec-envo x env-binding* rest env v)
  (conde
    ((== '() env-binding*)
     (parse-lookupo x rest v))
    ((fresh (p-name lam-expr env-binding-rest)
       (== `((rec . (,p-name . ,lam-expr)) . ,env-binding-rest) env-binding*)
       (conde
         ((== p-name x)
          (== `(closure ,lam-expr ,env) v))
         ((=/= p-name x)
          (parse-lookup-in-letrec-envo x env-binding-rest rest env v)))))))

(define parse-initial-env `((val . (car . (prim . car)))
                            (val . (cdr . (prim . cdr)))
                            (val . (null? . (prim . null?)))
                            (val . (pair? . (prim . pair?)))
                            (val . (symbol? . (prim . symbol?)))
                            (val . (number? . (prim . number?)))
                            (val . (procedure? . (prim . procedure?)))
                            (val . (cons . (prim . cons)))
                            (val . (not . (prim . not)))
                            (val . (equal? . (prim . equal?)))
                            (val . (list . (closure (lambda x x) ,empty-env)))
                            . ,empty-env))








;;; interpreter
(define (evalo expr val)
  (eval-expo expr initial-env val))

(define (eval-expo expr env val)
  (try-lookup-before expr env val (eval-expo-rest expr env val)))

(define (paramso params)
  (conde$-dfs
    ; Multiple argument
    ((list-of-paramso params))
    ; Variadic
    ((symbolo params))))

(define (eval-expo-rest expr env val)
  (lambdag@ (st)
    (let* ((expr (walk expr (state-S st)))
           (env (walk env (state-S st)))
           (depth (state-depth st))
           (goal (lambdag@ (st)
                   ((conde-weighted
    (5000 1 (conde$-dfs
              ((== `(quote ,val) expr)
               (absento 'closure val)
               (absento 'prim val)
               (not-in-envo 'quote env))

              ((numbero expr) (== expr val))

              ((boolean-primo expr val))

              ((fresh (rator rands rator-val)
                 (== `(,rator . ,rands) expr)
                 (eval-expo rator env rator-val)
                 (conde$-dfs
                   ((fresh (prim-id)
                      (== rator-val `(prim . ,prim-id))
                      (eval-primo prim-id val rands env)))
                   ((fresh (x body env^ a* res)
                      (== rator-val `(closure (lambda ,x ,body) ,env^))
                      (conde$-dfs
                        (;; Multi-argument
                         (same-length-ext-env*o x a* rands env^ res)
                         ; replacing eval-application with these may be faster with multi-level defer
                         ;(eval-expo body res val)
                         ;(eval-listo rands env a*)
                         (eval-application rands env a* (eval-expo body res val)))
                        (;; variadic
                         (symbolo x)
                         ;(project (rator) (lambdag@ (st) ((begin (display `(happened ,rator)) (newline) succeed) st)))
                         (== `((val . (,x . ,a*)) . ,env^) res)
                         (eval-expo body res val)
                         (eval-listo rands env a*))))))))))

    (#f #f (if-primo expr env val))

    (1 1 (fresh (x body)
       (== `(lambda ,x ,body) expr)
       (== `(closure (lambda ,x ,body) ,env) val)
       (paramso x)
       (not-in-envo 'lambda env)))

    ;; WEB 25 May 2016 -- This rather budget version of 'begin' is
    ;; useful for separating 'define' from the expression 'e',
    ;; specifically for purposes of Barliman.
    (1 1 (fresh (begin-body)
       (== `(begin . ,begin-body) expr)
       (not-in-envo 'begin env)
       (eval-begino '() begin-body env val)))

    (1 1 (fresh (b* letrec-body)
       (== `(letrec ,b* ,letrec-body) expr)
       (not-in-envo 'letrec env)
       (eval-letreco b* letrec-body env val)))

    (1 1 (cond-primo expr env val))

    (1 1 (handle-matcho expr env val))

    (1 1 (prim-expo expr env val))

    (1 1 (fresh (b* body)
           (== `(let ,b* ,body) expr)
           (not-in-envo 'let env)
           (let loop ((b* b*) (p* '()) (rand* '()))
             (conde
               ((fresh (a* res)
                  (== '() b*)
                  (ext-env*o p* a* env res)
                  (eval-application rand* env a* (eval-expo body res val))))
               ((fresh (p rand b*-rest)
                  (== `((,p ,rand) . ,b*-rest) b*)
                  (symbolo p)
                  (loop b*-rest (cons p p*) (cons rand rand*))))))))

    (1 1 (fresh (b* body)
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

    (1 1 (fresh (qq-expr)
           (== (list 'quasiquote qq-expr) expr)
           (not-in-envo 'quasiquote env)
           (eval-qq-expo qq-expr env val)))

    ) (state-depth-set st depth)))))

      (if (or (var? expr)
              (var? env)
              (and (pair? expr) (var? (walk (car expr) (state-S st)))))
        (state-deferred-defer st goal)
        (goal st)))))

(define (eval-qq-expo qq-expr env val)
  (conde
    ((fresh (expr)
       (== (list 'unquote expr) qq-expr)
       (eval-expo expr env val)))
    ((fresh (qq-a qq-d va vd)
       (== `(,qq-a . ,qq-d) qq-expr)
       (== `(,va . ,vd) val)
       (=/= 'unquote qq-a)
       (=/= 'closure qq-a)
       (=/= 'prim qq-a)
       (eval-qq-expo qq-a env va)
       (eval-qq-expo qq-d env vd)))
    ((== qq-expr val)
     (conde$
       ((== '() val))
       ((symbolo val))
       ((== #f val))
       ((== #t val))
       ((numbero val))))))

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
          (eval-letreco rec-defs e env val))))
    ((fresh (name args body begin-rest)
        (== `((define ,name (lambda ,args ,body)) . ,begin-rest) begin-body)
        (symbolo name)
        (eval-begino
          (cons `(,name (lambda ,args ,body)) rec-defs) begin-rest env val)))))

(define empty-env '())

(define (lookup-reco k renv x b* t)
    (conde
      ((== '() b*) (k))
      ((fresh (b*-rest p-name lam-expr)
         (== `((,p-name . ,lam-expr) . ,b*-rest) b*)
         (conde
           ((== p-name x) (== `(closure ,lam-expr ,renv) t))
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

(define (try-lookup-before x env t alts)
  (define (try-lookup-rec-before st renv b* alts)
    (let-values (((rb* vb*) (list-split-ground st b*)))
      (let loop ((rb* rb*)
                 (alts (if (null? vb*)
                         alts
                         (conde$
                           ((symbolo x)
                            (lookup-reco (lambda () alts) renv x vb* t))
                           (alts)))))
        (if (null? rb*) alts
          (let ((rib (car rb*)))
            (loop (cdr rb*)
                  (fresh (p-name lam-expr)
                    (== `(,p-name . ,lam-expr) rib)
                    (conde$
                      ((symbolo x)
                       (== p-name x)
                       (== `(closure ,lam-expr ,renv) t))
                      ((=/= p-name x) alts)))))))))
  (lambdag@ (st)
    (let-values (((rgenv venv) (list-split-ground st env)))
      (let loop ((rgenv rgenv)
                 (rec-env venv)
                 (alts (if (null? venv)
                         alts
                         (conde$ ;1$ (((x x)))
                           ((symbolo x) (lookupo x venv t))
                           (alts)))))
        (if (null? rgenv) (alts st)
          (let* ((rib (car rgenv)) (rec-env (cons rib rec-env)))
            (loop (cdr rgenv) rec-env
                  (conde$
                    ((fresh (y b)
                       (== `(val . (,y . ,b)) rib)
                       (conde$
                         ((symbolo x) (== x y) (== b t))
                         ((=/= x y) alts))))
                    ((fresh (b*)
                       (== `(rec . ,b*) rib)
                       (try-lookup-rec-before st rec-env b* alts)))))))))))

(define (not-in-envo x env)
  (conde1 (((x x) (env env)))
    ((== '() env))
    ((fresh (y b rest)
       (== `((val . (,y . ,b)) . ,rest) env)
       (=/= x y)
       (not-in-envo x rest)))
    ((fresh (b* rest)
       (== `((rec . ,b*) . ,rest) env)
       (not-in-env-reco x b* rest)))))

(define (not-in-env-reco x b* env)
  (conde1 (((x x) (b* b*)))
    ((== '() b*) (not-in-envo x env))
    ((fresh (p-name lam-expr b*-rest)
       (== `((,p-name . ,lam-expr) . ,b*-rest) b*)
       (=/= p-name x)
       (not-in-env-reco x b*-rest env)))))

(define (eval-listo expr env val)
  (conde1 (((expr expr)) ((val val)))
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
  (conde1 (((los los)))
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(define (not-in-paramso x params)
  (conde1 (((params params)))
    ((== '() params))
    ((fresh (a d)
       (== `(,a . ,d) params)
       (=/= a x)
       (not-in-paramso x d)))))

(define (list-of-paramso los)
  (conde1 (((los los)))
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-paramso d)
       (not-in-paramso a d)))))

(define (ext-env*o x* a* env out)
  (conde;1 (((x* x*)) ((a* a*)))
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((val . (,x . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

(define (same-length-ext-env*o x* a* r* env out)
  (conde
    ((== '() x*) (== '() a*) (== '() r*) (== env out))
    ((fresh (x a r dx* da* dr* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `(,r . ,dr*) r*)
       (== `((val . (,x . ,a)) . ,env) env2)
       (symbolo x)
       (same-length-ext-env*o dx* da* dr* env2 out)))))

(define (eval-primo prim-id val rands env)
  (project0 (prim-id val rands env)
    (conde$ ;1$ (((prim-id prim-id)))
      [(== prim-id 'cons)
       (fresh (a d)
         (== `(,a . ,d) val)
         (=/= 'closure a)
         (=/= 'prim a)
         (eval-listo rands env `(,a ,d)))]
      [(== prim-id 'car)
       (fresh (d)
         (=/= 'closure val)
         (=/= 'prim val)
         (eval-listo rands env `((,val . ,d))))]
      [(== prim-id 'cdr)
       (fresh (a)
         (=/= 'closure a)
         (=/= 'prim val)
         (eval-listo rands env `((,a . ,val))))]
      [(== prim-id 'null?)
       (fresh (v)
         (let ((assign-result (conde$
                                ((== '() v) (== #t val))
                                ((=/= '() v) (== #f val))))
               (eval-args (eval-listo rands env `(,v))))
           (if (var? val)
             (fresh () eval-args assign-result)
             (fresh () assign-result eval-args))))]
      [(== prim-id 'pair?)
       (fresh (v)
         (let ((assign-true (fresh (a d) (== #t val) (== `(,a . ,d) v) (=/= 'closure a) (=/= 'prim a)))
               (assign-false (fresh () (== #f val) (conde$
                                                     ((== '() v))
                                                     ((symbolo v))
                                                     ((== #f v))
                                                     ((== #t v))
                                                     ((numbero v))
                                                     ((fresh (d)
                                                        (== `(closure . ,d) v)))
                                                     ((fresh (d)
                                                        (== `(prim . ,d) v))))))
               (eval-args (eval-listo rands env `(,v))))
           (if (or (var? val) (eq? val #f))
             (fresh () eval-args (conde$ (assign-true) (assign-false)))
             (fresh () assign-true eval-args))))]
      [(== prim-id 'symbol?)
       (fresh (v)
         (let ((assign-true (fresh () (== #t val) (symbolo v)))
               (assign-false (fresh () (== #f val) (conde$
                                                     ((== '() v))
                                                     ((fresh (a d)
                                                        (== `(,a . ,d) v)))
                                                     ((== #f v))
                                                     ((== #t v))
                                                     ((numbero v)))))
               (eval-args (eval-listo rands env `(,v))))
           (if (or (var? val) (eq? val #f))
             (fresh () eval-args (conde$ (assign-true) (assign-false)))
             (fresh () assign-true eval-args))))]
      [(== prim-id 'number?)
       (fresh (v)
         (let ((assign-true (fresh () (== #t val) (numbero v)))
               (assign-false (fresh () (== #f val) (conde$
                                                     ((== '() v))
                                                     ((fresh (a d)
                                                        (== `(,a . ,d) v)))
                                                     ((== #f v))
                                                     ((== #t v))
                                                     ((symbolo v)))))
               (eval-args (eval-listo rands env `(,v))))
           (if (or (var? val) (eq? val #f))
             (fresh () eval-args (conde$ (assign-true) (assign-false)))
             (fresh () assign-true eval-args))))]
      [(== prim-id 'procedure?)
       (fresh (v)
         (let ((assign-true (fresh (d) (== #t val) (conde$
                                                     ((== `(closure . ,d) v))
                                                     ((== `(prim . ,d) v)))))
               (assign-false (fresh () (== #f val) (conde$
                                                    ((== '() v))
                                                    ((symbolo v))
                                                    ((== #f v))
                                                    ((== #t v))
                                                    ((numbero v))
                                                    ((fresh (a d)
                                                       (== `(,a . ,d) v)
                                                       (=/= 'closure a)
                                                       (=/= 'prim a))))))
               (eval-args (eval-listo rands env `(,v))))
           (if (or (var? val) (eq? val #f))
             (fresh () eval-args (conde$ (assign-true) (assign-false)))
             (fresh () assign-true eval-args))))]
      [(== prim-id 'not)
       (fresh (b)
         (let ((assign-result (conde$
                                ((== #f b) (== #t val))
                                ((=/= #f b) (== #f val))))
               (eval-args (eval-listo rands env `(,b))))
           (if (var? val)
             (fresh () eval-args assign-result)
             (fresh () assign-result eval-args))))]
      [(== prim-id 'equal?)
       (fresh (v1 v2)
         (let ((assign-result (conde$
                                ((== v1 v2) (== #t val))
                                ((=/= v1 v2) (== #f val))))
               (eval-args (eval-listo rands env `(,v1 ,v2))))
           (if (var? val)
             (fresh () eval-args assign-result)
             (fresh () assign-result eval-args))))]
      ;[(== prim-id 'list)
       ;(eval-listo rands env val)]
      )))

(define (prim-expo expr env val)
  (conde1$ (((expr expr)))
    ((and-primo expr env val))
    ((or-primo expr env val))))

(define (boolean-primo expr val)
  (conde1$ (((expr expr)) ((val val)))
    ((== #t expr) (== #t val))
    ((== #f expr) (== #f val))))

(define (and-primo expr env val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (not-in-envo 'and env)
    (ando e* env val)))

(define (ando e* env val)
  (conde1 (((e* e*)))
    ((== '() e*) (== #t val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde1 (((e1 e1) (env env)))
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
  (conde1 (((e* e*)))
    ((== '() e*) (== #f val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde1 (((e1 e1) (env env)))
         ((=/= #f v)
          (== v val)
          (eval-expo e1 env v))
         ((== #f v)
          (eval-expo e1 env v)
          (oro `(,e2 . ,e-rest) env val)))))))

;; Set this flag to #f to recover Scheme semantics.
(define boolean-conditions-only? #t)
(define (condition v)
  (if (and allow-incomplete-search? boolean-conditions-only?)
    (booleano v)
    unit))
(define (condition-true v)
  (if (and allow-incomplete-search? boolean-conditions-only?)
    (== #t v)
    (=/= #f v)))

(define (if-primo expr env val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (eval-expo e1 env t)
    (conde1 (((t t)))
      ((condition-true t) (eval-expo e2 env val))
      ((== #f t) (eval-expo e3 env val)))))

(define (cond-primo expr env val)
  (fresh (c c*)
    (== `(cond ,c . ,c*) expr)
    (not-in-envo 'cond env)
    (cond-clauseso `(,c . ,c*) env val)))

(define (cond-clauseso c* env val)
  (conde
    ((fresh (test conseq)
       (== `((,test ,conseq)) c*)
       (conde
         ((== 'else test)
          (not-in-envo 'else env)
          ;; the "real" 'else' auxilliary keyword,
          ;; not shadowed
          (eval-expo conseq env val))
         ((fresh (v)
            (condition v)
            (eval-expo test env v)
            ;; if test is 'else', it must have been shadowed
            (conde
              ((== #f v)
               ;; evaluate to an unspecified value!
               (== 'unspecified val))
              ((condition-true v) (eval-expo conseq env val))))))
       (eval-expo conseq env val)))
    ((fresh (test conseq c*-rest)
       (== `((,test ,conseq) . ,c*-rest) c*)
       (fresh (v)
         (condition v)
         (eval-expo test env v)
         (conde
           ((== #f v) (cond-clauseso c*-rest env val))
           ((condition-true v) (eval-expo conseq env val))))))))

(define initial-env `((val . (cons . (prim . cons)))
                      (val . (car . (prim . car)))
                      (val . (cdr . (prim . cdr)))
                      (val . (null? . (prim . null?)))
                      (val . (pair? . (prim . pair?)))
                      (val . (symbol? . (prim . symbol?)))
                      (val . (number? . (prim . number?)))
                      (val . (procedure? . (prim . procedure?)))
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
  (conde1$ (((t t)))
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (not-numbero t)
  (conde1$ (((t t)))
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (self-eval-literalo t)
  (conde1$ (((t t)))
    ((numbero t))
    ((booleano t))))

(define (literalo t)
  (conde1$ (((t t)))
    ((numbero t))
    ((symbolo t) (=/= 'closure t) (=/= 'prim t))
    ((booleano t))
    ((== '() t))))

(define (booleano t)
  (conde1$ (((t t)))
    ((== #f t))
    ((== #t t))))

(define (regular-env-appendo env1 env2 env-out)
  (conde1 (((env1 env1)))
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((val . (,y . ,v)) . ,rest) env1)
       (== `((val . (,y . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

(define (match-clauses mval clauses env val)
  (fresh (p result-expr d penv)
    (== `((,p ,result-expr) . ,d) clauses)
    (conde1 (((mval mval) (p p)))
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
    (=/= 'prim mval)
    (conde1 (((var var) (mval mval) (penv penv)))
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
  (conde1 (((p p) (mval mval)))
    ((self-eval-literalo p)
     (== p mval)
     (== penv penv-out))
    ((var-p-match p mval penv penv-out))
    ((fresh (datum)
       (== `(quote ,datum) p)
       (== datum mval)
       (== penv penv-out)))
    ((fresh (var pred val)
      (== `(? ,pred ,var) p)
      (conde1 (((pred pred)) ((mval mval)))
        ((== 'symbol? pred)
         (symbolo mval))
        ((== 'number? pred)
         (numbero mval)))
      (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-match quasi-p mval penv penv-out)))))

(define (p-no-match p mval penv penv-out)
  (conde1 (((p p) (mval mval)))
    ((self-eval-literalo p)
     (=/= p mval)
     (== penv penv-out))
    ((var-p-no-match p mval penv penv-out))
    ((fresh (datum)
       (== `(quote ,datum) p)
       (=/= datum mval)
       (== penv penv-out)))
    ((fresh (var pred val)
       (== `(? ,pred ,var) p)
       (== penv penv-out)
       (symbolo var)
       (conde1 (((pred pred)))
         ((== 'symbol? pred)
          (conde1 (((mval mval)))
            ((not-symbolo mval))
            ((symbolo mval)
             (var-p-no-match var mval penv penv-out))))
         ((== 'number? pred)
          (conde1 (((mval mval)))
            ((not-numbero mval))
            ((numbero mval)
             (var-p-no-match var mval penv penv-out)))))))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-no-match quasi-p mval penv penv-out)))))

(define (quasi-p-match quasi-p mval penv penv-out)
  (conde1 (((quasi-p quasi-p) (mval mval)))
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
  (conde1 (((quasi-p quasi-p) (mval mval)))
    ((=/= quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (=/= 'closure mval)
       (=/= 'prim mval)
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
       (conde1 (((a a)) ((v1 v1)))
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
       (symbolo name)
       (extract-nameso rest res)))))

(define (appendo l s out)
  (conde
    ((== '() l) (== s out))
    ((fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res)))))
