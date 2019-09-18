;; The definition of 'letrec' is based based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

(define (evalo expr val)
  (eval-expo expr initial-env val))

(define (eval-expo expr env val)
  (conde
    ((== `(quote ,val) expr)
     (absento 'closure val)
     (absento 'prim val)
     (absento 'num val)
     (not-in-envo 'quote env))
    
    ((numbero expr)
     (fresh (v)
       (s/declareo v)
       (== `(num ,v) val)
       (s/z3-alphao expr v)))

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

    ((fresh (x e body a env^)
       (== `(let ((,x ,e)) ,body) expr)
       (symbolo x)
       (ext-envo x a env env^)
       (eval-expo e env a)
       (eval-expo body env^ val)))
    
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

(define (ext-envo x a env out)
  (fresh ()
    (== `((,x . (val . ,a)) . ,env) out)
    (symbolo x)))

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
    ;; FIXME (webyrd) -- symbol?, and perhaps other type predicates, doesn't handle booleans (fails)
    [(== prim-id 'symbol?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((symbolo v) (== #t val))
         ((fresh (n)
            (s/declareo n)
            (== `(num ,n) v))
          (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)))))]
    [(== prim-id 'null?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((== '() v) (== #t val))
         ((=/= '() v) (== #f val))))]
    [(== prim-id 'zero?)
     (fresh (a1 n1)
       (== `(,a1) a*)
       (s/declareo n1)
       (== `(num ,n1) a1)
       (conde
         [(== #t val)
          (s/chas-zeroo n1)]
         [(== #f val)
          (conde
            [(s/chas-poso n1)]
            [(s/chasnt-poso n1) (s/chas-nego n1)])]))]
    [(== prim-id 'sub1)
     (fresh (a1 n1 n)
       (== `(,a1) a*)
       (s/declareo n1)
       (s/declareo n)
       (== `(num ,n1) a1)
       (== `(num ,n) val)
       ;; this code is super inefficient, and calls z3
       ;; a zillion time!  could be made faster
       (conde
         [(s/chas-poso n1)
          (s/chas-poso n) (s/chas-zeroo n)]
         [(s/chasnt-poso n1)
          (s/chasnt-poso n) (s/chasnt-zeroo n)])
       (conde
         [(s/chas-zeroo n1)
          (s/chas-nego n)]
         [(s/chasnt-zeroo n1) (s/chas-nego n1)
          (s/chas-nego n)]
         [(s/chasnt-zeroo n1) (s/chasnt-nego n1)
          (s/chasnt-nego n)]))]
    [(== prim-id '+)
     (fresh (a1 a2)
       (== `(,a1 ,a2) a*)
       (fresh (n1 n2 n)
         (s/declareo n1)
         (s/declareo n2)
         (s/declareo n)
         (== `(num ,n1) a1)
         (== `(num ,n2) a2)
         (== `(num ,n) val)
         ;; this call should probably comes before the arguments to
         ;; '+' are evaluated recursively.
         ;; this may improve divergence behavior
         (s/z3-plus-tableo n1 n2 n)))]
    [(== prim-id '*)
     (fresh (a1 a2)
       (== `(,a1 ,a2) a*)
       (fresh (n1 n2 n)
         (s/declareo n1)
         (s/declareo n2)
         (s/declareo n)
         (== `(num ,n1) a1)
         (== `(num ,n2) a2)
         (== `(num ,n) val)
         ;; this call should probably comes before the arguments to
         ;; '*' are evaluated recursively.
         ;; this may improve divergence behavior
         (s/z3-times-tableo n1 n2 n)))]))

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
                      (zero? . (val . (prim . zero?)))
                      (sub1 . (val . (prim . sub1)))
                      (+ . (val . (prim . +)))
                      (* . (val . (prim . *)))
                      . ,empty-env))
