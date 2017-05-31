;; The definition of 'letrec' is based based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

(define closure-tag (gensym "#%closure"))
(define prim-tag (gensym "#%primitive"))
(define undefined-tag (gensym "#%undefined"))


;; TODO: defer conde fallbacks during eager evaluation.
;; TODO: fresh should push/pop deferred context for hierarchical binding.
;; TODO: reverse eval-listo queuing order once deferring works.

(define (literal-expo expr env val) (== expr val))

(define (quote-primo expr env val)
  (fresh ()
    (== `(quote ,val) expr)
    (absento closure-tag val)
    (absento prim-tag val)
    (not-in-envo 'quote env)))

(define (lambda-primo expr env val)
  (fresh (x body)
    (== `(lambda ,x ,body) expr)
    (== `(,closure-tag (lambda ,x ,body) ,env) val)
    (paramso x)
    (not-in-envo 'lambda env)))

(define (app-closure-variadico expr env val)
  (fresh (rator x rands body env^ a*)
    (== `(,rator . ,rands) expr)
    (eval-expo rator env `(,closure-tag (lambda ,x ,body) ,env^))
    (ext-env1-evalo x a* env^ body val)
    (eval-listo rands env a*)))

(define (app-closure-multi-argo expr env val)
  (fresh (rator x* rands body env^ a*)
    (== `(,rator . ,rands) expr)
    (eval-expo rator env `(,closure-tag (lambda ,x* ,body) ,env^))
    (eval-listo rands env a*)
    (ext-env*-evalo x* rands a* env^ body val)))

(define (app-primo expr env val)
  (fresh (rator rands a* prim-id)
    (== `(,rator . ,rands) expr)
    (eval-expo rator env `(,prim-tag . ,prim-id))
    (eval-primo prim-id a* val)
    (eval-listo rands env a*)))

(define (app-expo expr env val)
  (fresh (rator rand* op a*)
    (== `(,rator . ,rand*) expr)
    (let-deferred
      (rator-deferred (eval-expo rator env op))
      (project0 (op)
        (cond
          ((var? op)
           ;; TODO: factor out eval-listo rands.
           (fresh ()
             (eval-listo rand* env a*)
             (defer
               (conde
                 ((fresh (x body env^)
                    (== `(,closure-tag (lambda ,x ,body) ,env^) op)
                    (ext-env1-evalo x a* env^ body val)))
                 ((fresh (x* body env^)
                    (== `(,closure-tag (lambda ,x* ,body) ,env^) op)
                    (ext-env*-evalo x* rand* a* env^ body val)))
                 ((fresh (x* rand* prim-id)
                    (== `(,prim-tag . ,prim-id) op)
                    (eval-primo prim-id a* val)))))
             (defer* rator-deferred)))
          ((pair? op)
           ;; Assume a procedure "pair" must have been immediately tagged.
           (cond
             ((eq? closure-tag (car op))
              (fresh (params body env^)
                (== `((lambda ,params ,body) ,env^) (cdr op))
                (eval-listo rand* env a*)
                (defer* rator-deferred)
                (project0 (params)
                  (cond
                    ((var? params)
                     (conde
                       ((ext-env1-evalo params a* env^ body val))
                       ((ext-env*-evalo params rand* a* env^ body val))))
                    ((pair? params) (ext-env*-evalo params rand* a* env^ body val))
                    (else (ext-env1-evalo params a* env^ body val))))))
             ((eq? prim-tag (car op))
              (fresh ()
                (eval-primo (cdr op) a* val)
                (eval-listo rand* env a*)
                (defer* rator-deferred)))
             (else fail)))
          (else fail))))))

(define (begin-primo expr env val)
  (fresh (begin-body)
    (== `(begin . ,begin-body) expr)
    (not-in-envo 'begin env)
    (eval-begino '() begin-body env val)))

(define (letrec-primo expr env val)
  (fresh (b* letrec-body)
    (== `(letrec ,b* ,letrec-body) expr)
    (not-in-envo 'letrec env)
    (eval-letreco b* letrec-body env val)))

(define (let-primo expr env val)
  (fresh (b* body)
    ;; TODO: ensure bound names are all different
    (== `(let ,b* ,body) expr)
    (not-in-envo 'let env)
    (let loop ((b* b*) (p* '()) (rand* '()))
      (define (gnull)
        (fresh (a* res)
          (eval-listo rand* env a*)
          (ext-env*-evalo p* rand* a* env body val)))
      (define (gpair)
        (fresh (p rand b*-rest)
          (== `((,p ,rand) . ,b*-rest) b*)
          (symbolo p)
          (loop b*-rest (cons p p*) (cons rand rand*))))
      (project0 (b*)
        (cond
          ((null? b*) (gnull))
          ((pair? b*) (gpair))
          (else
            (conde ((== '() b*) (gnull)) ((gpair)))))))))

(define (let*-primo expr env val)
  (fresh (b* body)
    (== `(let* ,b* ,body) expr)
    (not-in-envo 'let env)
    (let loop ((b* b*) (env env))
      (define (gpair)
        (fresh (p rand a b*-rest res)
          (== `((,p ,rand) . ,b*-rest) b*)
          (ext-env1o p a env res)
          (loop b*-rest res)
          (eval-expo rand env a)))
      (project0 (b*)
        (cond
          ((null? b*) (eval-expo body env val))
          ((pair? b*) (gpair))
          (else
            (conde
              ((== '() b*) (eval-expo body env val))
              ((gpair)))))))))

(define (quasiquote-primo expr env val)
  (fresh (qq-expr)
    (== (list 'quasiquote qq-expr) expr)
    (not-in-envo 'quasiquote env)
    (eval-qq-expo 0 qq-expr env val)))


(define (case-bound-symbol-reco x b* env loop gk-unknown gk-unbound gk-else)
  (project0 (b*)
    (cond
      ((var? b*) (loop env gk-unknown))
      ((null? b*) (loop env gk-unbound))
      (else
        (fresh (p-name lam-expr b*-rest)
          (== `((,p-name . ,lam-expr) . ,b*-rest) b*)
          (project0 (p-name)
            (cond
              ((eq? p-name x) (gk-else))
              ((var? p-name)
               (case-bound-symbol-reco
                 x b*-rest env loop gk-unknown gk-unknown gk-else))
              (else
                (case-bound-symbol-reco
                  x b*-rest env loop gk-unknown gk-unbound gk-else)))))))))
(define (case-bound-symbolo x env gk-unknown gk-unbound gk-else)
  (cond
    ((var? x) (gk-unknown))
    ((not (symbol? x)) (gk-else))
    (else
      (let loop ((env env) (gk-unbound gk-unbound))
        (project0 (env)
          (cond
            ((var? env) (gk-unknown))
            ((null? env) (gk-unbound))
            (else
              (fresh (tag rib rest)
                (== `((,tag . ,rib) . ,rest) env)
                (project0 (tag)
                  (cond
                    ((var? tag) (loop rest gk-unknown))
                    ((eq? 'rec tag)
                     (case-bound-symbol-reco
                       x rib rest loop gk-unknown gk-unbound gk-else))
                    ((eq? 'val tag)
                     (fresh (y b)
                       (== `(,y . ,b) rib)
                       (project0 (y)
                         (cond
                           ((var? y) (loop rest gk-unknown))
                           ((eq? y x) (gk-else))
                           (else (loop rest gk-unbound))))))
                    (else fail)))))))))))


(define (evalo expr val)
  (eval-expo expr initial-env val))

(define (eval-expo expr env val)
  (define (gall)
    (lookupo-alt
      expr env val
      (conde
        ((quote-primo expr env val))
        ((numbero expr) (literal-expo expr env val))
        ((lambda-primo expr env val))
        ((app-closure-variadico expr env val))
        ((app-closure-multi-argo expr env val))
        ((app-primo expr env val))
        ((handle-matcho expr env val))
        ((begin-primo expr env val))
        ((letrec-primo expr env val))
        ((let-primo expr env val))
        ((let*-primo expr env val))
        ((quasiquote-primo expr env val))
        ((cond-primo expr env val))
        ((boolean-primo expr env val))
        ((and-primo expr env val))
        ((or-primo expr env val))
        ((if-primo expr env val)))))
  (define (gpair)
    (conde
      ((quote-primo expr env val))
      ((lambda-primo expr env val))
      ((app-closure-variadico expr env val))
      ((app-closure-multi-argo expr env val))
      ((app-primo expr env val))
      ((handle-matcho expr env val))
      ((begin-primo expr env val))
      ((letrec-primo expr env val))
      ((let-primo expr env val))
      ((let*-primo expr env val))
      ((quasiquote-primo expr env val))
      ((cond-primo expr env val))
      ((and-primo expr env val))
      ((or-primo expr env val))
      ((if-primo expr env val))))

  (project0 (expr)
    (cond
      ((or (not expr) (eq? #t expr) (number? expr))
       (literal-expo expr env val))
      ((symbol? expr) (lookupo expr env val))
      ((pair? expr)
       (let ((ea (car expr)))
         (project0 (ea)
           (case-bound-symbolo
             ea env gpair
             (lambda ()
               (case ea
                 ((quote) (quote-primo expr env val))
                 ((lambda) (lambda-primo expr env val))
                 ((if) (if-primo expr env val))
                 ((begin) (begin-primo expr env val))
                 ((letrec) (letrec-primo expr env val))
                 ((let) (let-primo expr env val))
                 ((let*) (let*-primo expr env val))
                 ((quasiquote) (quasiquote-primo expr env val))
                 ((cond) (cond-primo expr env val))
                 ((and) (and-primo expr env val))
                 ((or) (or-primo expr env val))
                 ((match) (handle-matcho expr env val))
                 (else fail)))
             (lambda () (app-expo expr env val))))))
      ((var? expr) (gall))
      (else fail))))

(define empty-env '())

(define (lookup-reco k renv x b* t)
  (define (gtest b*-rest p-name lam-expr)
    (conde
      ((== p-name x) (== `(,closure-tag ,lam-expr ,renv) t))
      ((=/= p-name x) (lookup-reco k renv x b*-rest t))))
  (define (gpair)
    (fresh (b*-rest p-name lam-expr)
      (== `((,p-name . ,lam-expr) . ,b*-rest) b*)
      (gtest b*-rest p-name lam-expr)))
  (project0 (b*)
    (cond
      ((null? b*) (k))
      ((pair? b*)
       (fresh (b*-rest p-name lam-expr)
         (== `((,p-name . ,lam-expr) . ,b*-rest) b*)
         (project0 (x p-name)
           (if (and (symbol? x) (symbol? p-name))
             (if (eq? x p-name)
               (== `(,closure-tag ,lam-expr ,renv) t)
               (lookup-reco k renv x b*-rest t))
             (gtest b*-rest p-name lam-expr)))))
      ((var? b*) (conde ((== '() b*) (k)) ((gpair))))
      (else fail))))

(define (lookupo-alt x env t galt)
  (define (gtest y b rest)
    (conde ((== x y) (== b t)) ((=/= x y) (lookupo-alt x rest t galt))))
  (define (gval)
    (fresh (y b rest)
      (== `((val . (,y . ,b)) . ,rest) env)
      (gtest y b rest)))
  (define (grec)
    (fresh (b* rest)
      (== `((rec . ,b*) . ,rest) env)
      (lookup-reco (lambda () (lookupo-alt x rest t galt)) env x b* t)))
  (define (gdefault) (conde ((gval)) ((grec)) ((== '() env) galt)))
  (project0 (env)
    (cond
      ((pair? env)
       (let ((rib (car env)))
         (project0 (rib)
           (cond
             ((pair? rib)
              (let ((tag (car rib)))
                (project0 (tag)
                  (if (var? tag)
                    (gdefault)
                    (case tag
                      ((val)
                       (fresh (y b rest)
                         (== `((val . (,y . ,b)) . ,rest) env)
                         (project0 (x y)
                           (if (and (symbol? x) (symbol? y))
                             (if (eq? x y)
                               (== b t)
                               (lookupo-alt x rest t galt))
                             (gtest y b rest)))))
                      ((rec) (grec))
                      (else fail))))))
             (else (gdefault))))))
      (else (gdefault)))))

(define (lookupo x env t) (lookupo-alt x env t fail))

(define (not-in-envo x env)
  (define (gval)
    (fresh (y b rest)
      (== `((val . (,y . ,b)) . ,rest) env)
      (=/= x y)
      (not-in-envo x rest)))
  (define (grec)
    (fresh (b* rest)
      (== `((rec . ,b*) . ,rest) env)
      (not-in-env-reco x b* rest)))
  (project0 (env)
    (cond
      ((null? env) succeed)
      ((pair? env)
       (let ((rib (car env)))
         (project0 (rib)
           (cond
             ((pair? rib)
              (let ((tag (car rib)))
                (project0 (tag)
                  (if (var? tag)
                    (conde ((gval)) ((grec)))
                    (case tag
                      ((val) (gval))
                      ((rec) (grec))
                      (else fail))))))
             (else (conde ((gval)) ((grec))))))))
      ((var? env) (conde ((== empty-env env)) ((gval)) ((grec))))
      (else fail))))

(define (not-in-env-reco x b* env)
  (define (gpair)
    (fresh (p-name lam-expr b*-rest)
      (== `((,p-name . ,lam-expr) . ,b*-rest) b*)
      (=/= p-name x)
      (not-in-env-reco x b*-rest env)))
  (project0 (b*)
    (cond
      ((null? b*) (not-in-envo x env))
      ((pair? b*) (gpair))
      ((var? b*) (conde ((== '() b*) (not-in-envo x env)) ((gpair))))
      (else fail))))

(define (eval-letreco b* letrec-body env val)
  (let loop ((b* b*) (rb* '()))
    (define (gdefs)
      (fresh (p-name x body b*-rest)
        (== `((,p-name (lambda ,x ,body)) . ,b*-rest) b*)
        (symbolo p-name)
        (paramso x)
        (loop b*-rest `((,p-name . (lambda ,x ,body)) . ,rb*))))
    (project0 (b*)
      (cond
        ((null? b*) (eval-expo letrec-body `((rec . ,rb*) . ,env) val))
        ((pair? b*) (gdefs))
        (else
          (conde
            ((== '() b*) (eval-expo letrec-body `((rec . ,rb*) . ,env) val))
            ((gdefs))))))))

;; NOTE: rec-defs is Scheme state, not a logic term!
(define (eval-begino rec-defs begin-body env val)
  (define (gbody e)
    (if (null? rec-defs)
      (eval-expo e env val)
      (eval-letreco rec-defs e env val)))
  (define (gdefine e begin-rest)
    (fresh (name args body)
      (== `(define ,name (lambda ,args ,body)) e)
      (symbolo name)
      (eval-begino
        (cons `(,name (lambda ,args ,body)) rec-defs) begin-rest env val)))
  (fresh (e begin-rest)
    (== `(,e . ,begin-rest) begin-body)
    (project0 (begin-rest)
      (cond
        ((null? begin-rest) (gbody e))
        ((pair? begin-rest) (gdefine e begin-rest))
        (else
          (conde
            ((== '() begin-rest) (gbody e))
            ((gdefine e begin-rest))))))))

;; 'level' is non-relational.
(define (eval-qq-expo level qq-expr env val)
  (define (guq)
    (fresh (expr)
      (== (list 'unquote expr) qq-expr)
      (if (= 0 level)
        (eval-expo expr env val)
        (fresh (sub-val)
          (== (list 'unquote sub-val) val)
          (eval-qq-expo (- level 1) expr env sub-val)))))
  (define (gqq)
    (fresh (expr sub-val)
      (== (list 'quasiquote expr) qq-expr)
      (== (list 'quasiquote sub-val) val)
      (eval-qq-expo (+ level 1) expr env sub-val)))
  (define (gpair)
    (fresh (qq-a qq-d va vd)
      (== `(,qq-a . ,qq-d) qq-expr)
      (== `(,va . ,vd) val)
      (=/= 'unquote qq-a)
      (=/= 'quasiquote qq-a)
      (=/= closure-tag qq-a)
      (=/= prim-tag qq-a)
      (eval-qq-expo level qq-a env va)
      (eval-qq-expo level qq-d env vd)))
  (project0 (qq-expr)
    (cond
      ((var? qq-expr)
       (conde ((guq)) ((gqq)) ((gpair))
         ((== qq-expr val)
          (conde
            ((== '() val))
            ((symbolo val))
            ((== #f val))
            ((== #t val))
            ((numbero val))))))
      ((pair? qq-expr)
       (fresh (qq-a qq-d)
         (== `(,qq-a . ,qq-d) qq-expr)
         (project0 (qq-a)
           (cond
             ((var? qq-a) (conde ((guq)) ((gqq)) ((gpair))))
             ((eq? 'unquote qq-a) (guq))
             ((eq? 'quasiquote qq-a) (gqq))
             (else (gpair))))))
      (else (== qq-expr val)))))

(define (eval-listo expr env val)
  (define (gpair)
    (fresh (a d v-a v-d)
      (== `(,a . ,d) expr)
      (== `(,v-a . ,v-d) val)
      (eval-expo a env v-a)
      (eval-listo d env v-d)))
  (project0 (expr)
    (cond
      ((null? expr) (== '() val))
      ((pair? expr) (gpair))
      (else (conde ((== '() expr) (== '() val)) ((gpair)))))))

(define (paramso params)
  (let loop ((params params) (seen '()))
    (define (not-seen name)
      (let seen-loop ((seen seen))
        (if (null? seen)
          succeed
          (fresh ()
            (=/= (car seen) name)
            (seen-loop (cdr seen))))))
    (project0 (params)
      (cond
        ;; Don't bother deferring, env extension can handle the rest.
        ((or (null? params) (var? params)) succeed)
        ((symbol? params) (not-seen params))
        ((pair? params)
         (fresh ()
           (symbolo (car params))
           (not-seen (car params))
           (loop (cdr params) (cons (car params) seen))))
        (else fail)))))

(define (ext-env1o x a* env out)
  (fresh ()
    (symbolo x)
    (== `((val . (,x . ,a*)) . ,env) out)))

;; TODO: ensure uniqueness of names here instead of in paramso.
(define (ext-env*o-gps x* r* a* env out gk)
  (define (gnil)
    (fresh () (== '() x*) (== '() r*) (== '() a*) (== env out) gk))
  (define (gpair)
    (fresh (x r a dx* dr* da* env2)
      (== `(,x . ,dx*) x*)
      (== `(,r . ,dr*) r*)
      (== `(,a . ,da*) a*)
      (== `((val . (,x . ,a)) . ,env) env2)
      (symbolo x)
      (ext-env*o-gps dx* dr* da* env2 out gk)))
  (project0 (x* r* a*)
    (cond
      ((or (null? x*) (null? r*) (null? a*)) (gnil))
      ((or (pair? x*) (pair? r*) (pair? a*)) (gpair))
      ((and (var? x*) (var? r*) (var? a*)) (conde ((gnil)) ((gpair))))
      (else fail))))

(define (ext-env1-evalo param a* env body val)
  (fresh (res) (ext-env1o param a* env res) (eval-expo body res val)))

(define (ext-env*-evalo params rand* a* env body val)
  (fresh (res)
    (ext-env*o-gps params rand* a* env res (eval-expo body res val))))

(define (eval-primo prim-id a* val)
  (define (gcons)
    (fresh (a d)
      (== `(,a ,d) a*)
      (== `(,a . ,d) val)
      (=/= closure-tag a)
      (=/= prim-tag a)))
  (define (gcar)
    (fresh (d)
      (== `((,val . ,d)) a*)
      (=/= closure-tag val)
      (=/= prim-tag val)))
  (define (gcdr)
    (fresh (a)
      (== `((,a . ,val)) a*)
      (=/= closure-tag a)
      (=/= prim-tag a)))
  (define (gnot)
    (fresh (b)
      (== `(,b) a*)
      (project0 (b)
        (cond
          ((var? b)
           (conde ((=/= #f b) (== #f val)) ((== #f b) (== #t val))))
          ((not b) (== #t val))
          (else (== #f val))))))
  (define (gequal?)
    (fresh (v1 v2)
      (== `(,v1 ,v2) a*)
      (lambda (st)
        ((let ((st0 (state-with-scope st nonlocal-scope)))
           (let ((==? ((== v1 v2) st0)) (=/=? ((=/= v1 v2) st0)))
             (if ==?
               (if =/=?
                 (conde ((== v1 v2) (== #t val)) ((=/= v1 v2) (== #f val)))
                 (fresh () (== v1 v2) (== #t val)))
               (fresh () (=/= v1 v2) (== #f val))))) st))))
  (define (gsymbol?)
    (fresh (v)
      (== `(,v) a*)
      (project0 (v)
        (cond
          ((var? v)
           (conde
             ((== #t val) (symbolo v))
             ((== #f val)
              (conde
                ((== '() v))
                ((== #f v))
                ((== #t v))
                ((numbero v))
                ((fresh (a d) (== `(,a . ,d) v)))))))
          ((symbol? v) (== #t val))
          (else (== #f val))))))
  (define (gnumber?)
    (fresh (v)
      (== `(,v) a*)
      (project0 (v)
        (cond
          ((var? v)
           (conde
             ((== #t val) (numbero v))
             ((== #f val)
              (conde
                ((== '() v))
                ((== #f v))
                ((== #t v))
                ((symbolo v))
                ((fresh (a d) (== `(,a . ,d) v)))))))
          ((number? v) (== #t val))
          (else (== #f val))))))
  (define (gnull?)
    (fresh (v)
      (== `(,v) a*)
      (project0 (v)
        (cond
          ((var? v) (conde ((== '() v) (== #t val)) ((=/= '() v) (== #f val))))
          ((null? v) (== #t val))
          (else (== #f val))))))
  (define (gpair?)
    (fresh (v)
      (== `(,v) a*)
      (project0 (v)
        (cond
          ((var? v)
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
                ((fresh (d) (== `(,prim-tag . ,d) v)))))))
          ((pair? v)
           (fresh (a d)
             (== `(,a . ,d) v)
             (project0 (a)
               (cond
                 ((var? a)
                  (conde
                    ((fresh ()
                       (== #t val)
                       (=/= closure-tag a)
                       (=/= prim-tag a)))
                    ((== #f val)
                     (conde
                       ((== `(,closure-tag . ,d) v))
                       ((== `(,prim-tag . ,d) v))))))
                 ((or (eq? closure-tag a) (eq? prim-tag a)) (== #f val))
                 (else (== #t val))))))
           (else (== #f val))))))
  (define (gprocedure?)
    (fresh (v)
      (== `(,v) a*)
      (project0 (v)
        (cond
          ((var? v)
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
                   (=/= prim-tag a)))))))
          ((pair? v)
           (fresh (a d)
             (== `(,a . ,d) v)
             (project0 (a)
               (cond
                 ((var? a)
                  (conde
                    ((== #t val)
                     (conde
                       ((== `(,closure-tag . ,d) v))
                       ((== `(,prim-tag . ,d) v))))
                    ((fresh ()
                       (== #f val)
                       (=/= closure-tag a)
                       (=/= prim-tag a)))))
                 ((or (eq? closure-tag a) (eq? prim-tag a)) (== #t val))
                 (else (== #t val))))))
          (else (== #f val))))))

  (project0 (prim-id)
    (if (var? prim-id)
      (conde
        [(== prim-id 'cons) (gcons)]
        [(== prim-id 'car) (gcar)]
        [(== prim-id 'cdr) (gcdr)]
        [(== prim-id 'not) (gnot)]
        [(== prim-id 'equal?) (gequal?)]
        [(== prim-id 'symbol?) (gsymbol?)]
        [(== prim-id 'number?) (gnumber?)]
        [(== prim-id 'null?) (gnull?)]
        [(== prim-id 'pair?) (gpair?)]
        [(== prim-id 'procedure?) (gprocedure?)])
      (case prim-id
        ((cons) (gcons))
        ((car) (gcar))
        ((cdr) (gcdr))
        ((not) (gnot))
        ((equal?) (gequal?))
        ((symbol?) (gsymbol?))
        ((number?) (gnumber?))
        ((null?) (gnull?))
        ((pair?) (gpair?))
        ((procedure?) (gprocedure?))
        (else fail)))))

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
  (define (gmulti)
    (fresh (e1 e2 e-rest v)
      (== `(,e1 ,e2 . ,e-rest) e*)
      (eval-expo e1 env v)
      (project0 (v)
        (cond
          ((var? v)
           (conde
             ((== #f v) (== #f val))
             ((=/= #f v) (ando `(,e2 . ,e-rest) env val))))
          ((not v) (== #f val))
          (else (ando `(,e2 . ,e-rest) env val))))))
  (project0 (e*)
    (cond
      ((null? e*) (== #t val))
      ((pair? e*)
       (fresh (ea ed)
         (== `(,ea . ,ed) e*)
         (project0 (ed)
           (cond
             ((null? ed) (eval-expo ea env val))
             ((pair? ed) (gmulti))
             (else
               (conde
                 ((fresh (e) (== `(,e) e*) (eval-expo e env val)))
                 ((gmulti))))))))
      (else
        (conde
          ((== '() e*) (== #t val))
          ((fresh (e) (== `(,e) e*) (eval-expo e env val)))
          ((gmulti)))))))

(define (or-primo expr env val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (not-in-envo 'or env)
    (oro e* env val)))

(define (oro e* env val)
  (define (gmulti)
    (fresh (e1 e2 e-rest v)
      (== `(,e1 ,e2 . ,e-rest) e*)
      (eval-expo e1 env v)
      (project0 (v)
        (cond
          ((var? v)
           (conde
             ((=/= #f v) (== v val))
             ((== #f v) (oro `(,e2 . ,e-rest) env val))))
          ((not v) (oro `(,e2 . ,e-rest) env val))
          (else (== v val))))))
  (project0 (e*)
    (cond
      ((null? e*) (== #f val))
      ((pair? e*)
       (fresh (ea ed)
         (== `(,ea . ,ed) e*)
         (project0 (ed)
           (cond
             ((null? ed) (eval-expo ea env val))
             ((pair? ed) (gmulti))
             (else
               (conde
                 ((fresh (e) (== `(,e) e*) (eval-expo e env val)))
                 ((gmulti))))))))
      (else
        (conde
          ((== '() e*) (== #f val))
          ((fresh (e) (== `(,e) e*) (eval-expo e env val)))
          ((gmulti)))))))

(define (if-primo expr env val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (eval-expo e1 env t)
    (project0 (t)
      (cond
        ((var? t)
         (conde
           ((=/= #f t) (eval-expo e2 env val))
           ((== #f t) (eval-expo e3 env val))))
        ((not t) (eval-expo e3 env val))
        (else (eval-expo e2 env val))))))

(define (cond-primo expr env val)
  (fresh (c c*)
    (== `(cond ,c . ,c*) expr)
    (not-in-envo 'cond env)
    (cond-clauseso `(,c . ,c*) env val)))

(define (cond-clauseso c* env val)
  (define (gelse)
    (fresh (conseq)
      (== `((else ,conseq)) c*)
      (not-in-envo 'else env)
      (eval-expo conseq env val)))
  (define (gtest)
    (fresh (test conseq c*-rest)
      (== `((,test ,conseq) . ,c*-rest) c*)
      (fresh (v)
        (eval-expo test env v)
        (project0 (v)
          (cond
            ((var? v)
             (conde
               ((=/= #f v) (eval-expo conseq env val))
               ((== #f v) (cond-clauseso c*-rest env val))))
            ((not v) (cond-clauseso c*-rest env val))
            (else (eval-expo conseq env val)))))))
  (project0 (c*)
    (cond
      ((null? c*) (== undefined-tag val))
      ((pair? c*)
       (fresh (test conseq cd)
         (== `((,test ,conseq) . ,cd) c*)
         (project0 (cd)
           (cond
             ((null? cd)
              (project0 (test)
                (if (eq? 'else test)
                  (case-bound-symbolo
                    test env
                    (lambda () (conde ((gelse)) ((gtest))))
                    (lambda () (eval-expo conseq env val))
                    (lambda () (gtest)))
                  (gtest))))
             ((pair? cd) (gtest))
             (else (conde ((gelse)) ((gtest))))))))
      (else
        (conde
          ((== '() c*) (== undefined-tag val))
          ((gelse))
          ((gtest)))))))


(define initial-env `((val . (list . (,closure-tag (lambda x x) ,empty-env)))
                      (val . (not . (,prim-tag . not)))
                      (val . (equal? . (,prim-tag . equal?)))
                      (val . (symbol? . (,prim-tag . symbol?)))
                      (val . (number? . (,prim-tag . number?)))
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
