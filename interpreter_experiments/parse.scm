;;; 14 June 02016
;;;
;;; relational parser for 'miniScheme' with 'match'

;; > (run 1000 (q) (parseo '(begin (define append (lambda (x) x)) (append 5))))
;; (_.0)
;; > (run 1000 (q) (parseo '(begin (define append (lambda (x) x)) (appen 5))))
;; ()
;; > (run 1000 (q) (parseo '(begin (define append (lambda (x) x)) (append 5))))
;; (_.0)
;; > (run 1000 (q) (parseo '(begin (defin append (lambda (x) x)) (append 5))))
;; ()

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
       (lookupo expr env val)))

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
       (parse-listo `(,rator . ,rands) env)
       ;; TODO -- need to do anything special to handle prim application?
       ))

;; TODO
;;    ((parse-matcho expr env))

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
