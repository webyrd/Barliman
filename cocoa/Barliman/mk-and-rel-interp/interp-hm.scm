(load "mk/mk-vicare.scm")
(load "mk/mk.scm")

(define closure-tag (gensym "#%closure"))
;; The non-gensym tag is just for nicer pretty printing.
;(define closure-tag '#%closure)

(define evalo
  (lambda (exp type val)
    (eval-expo exp '() type val)))
(define :o
  (lambda (exp type) (:-expo exp '() type)))

(define eval-expo
  (lambda (exp env ty val)
    (conde
      ((symbolo exp) (lookupo exp env ty val))
      ((fresh (rator rand x body env^ a tyx tybody)
         (== `(,rator ,rand) exp)
         (eval-expo
           rator env `(-> ,tyx ,tybody) `(,closure-tag ,x ,body ,env^))
         (eval-expo rand env tyx a)
         (eval-expo
           body `((lambda (,x ,tyx . ,a)) . ,env^) tybody val)))
      ((fresh (x body tyx tybody v)
         (== `(lambda (,x) ,body) exp)
         (symbolo x)
         (not-in-envo 'lambda env)
         (== `(-> ,tyx ,tybody) ty)
         (== `(,closure-tag ,x ,body ,env) val)))
      ((fresh (b* body)
         (== `(letrec ,b* ,body) exp)
         (not-in-envo 'letrec env)
         (eval-letreco b* body env ty val)))
      ((fresh (b* body)
         (== `(let ,b* ,body) exp)
         (not-in-envo 'let env)
         (eval-leto b* body env ty val)))
      ((fresh (a d va vd ta td)
         (== `(cons ,a ,d) exp)
         (not-in-envo 'cons env)
         (== `(,va . ,vd) val)
         (== `(cons ,ta ,td) ty)
         (eval-expo a env ta va)
         (eval-expo d env td vd)))
      ((fresh (c a d tc td)
         (== `(car ,c) exp)
         (not-in-envo 'car env)
         (== a val)
         (== `(cons ,ty ,td) tc)
         (eval-expo c env tc `(,a . ,d))))
      ((fresh (c a d tc ta)
         (== `(cdr ,c) exp)
         (not-in-envo 'cdr env)
         (== d val)
         (== `(cons ,ta ,ty) tc)
         (eval-expo c env tc `(,a . ,d))))
      ((== `(quote ,val) exp)
       (not-in-envo 'quote env)
       (absento closure-tag val)
       (eval-quoteo ty val))
      ((eval-literalo exp ty val)))))

(define :-expo
  (lambda (exp env ty)
    (conde
      ((symbolo exp) (fresh (v) (lookupo exp env ty v)))
      ((fresh (rator rand tyx tybody)
         (== `(,rator ,rand) exp)
         (:-expo rator env `(-> ,tyx ,ty))
         (:-expo rand env tyx)))
      ((fresh (x body v tyx tybody)
         (== `(lambda (,x) ,body) exp)
         (symbolo x)
         (not-in-envo 'lambda env)
         (:-lambdao x body env ty)))
      ((fresh (b* body)
         (== `(letrec ,b* ,body) exp)
         (not-in-envo 'letrec env)
         (:-letreco b* body env ty)))
      ((fresh (b* body)
         (== `(let ,b* ,body) exp)
         (not-in-envo 'let env)
         (:-leto b* body env ty)))
      ((fresh (a d ta td)
         (== `(cons ,a ,d) exp)
         (== `(cons ,ta ,td) ty)
         (not-in-envo 'cons env)
         (:-expo a env ta)
         (:-expo d env td)))
      ((fresh (c tc td)
         (== `(car ,c) exp)
         (not-in-envo 'car env)
         (== `(cons ,ty ,td) tc)
         (:-expo c env tc)))
      ((fresh (c tc ta)
         (== `(cdr ,c) exp)
         (not-in-envo 'cdr env)
         (== `(cons ,ta ,ty) tc)
         (:-expo c env tc)))
      ((fresh (datum)
         (== `(quote ,datum) exp)
         (not-in-envo 'quote env)
         (absento closure-tag datum)
         (:-quoteo datum ty)))
      ((:-literalo exp ty)))))

(define :-literalo
  (lambda (datum ty)
    (conde
      ((== 'num ty) (numbero datum))
      ((== 'bool ty) (== #t datum))
      ((== 'bool ty) (== #f datum)))))

(define :-quoteo
  (lambda (datum ty)
    (conde
      ((== 'sym ty) (symbolo datum))
      ((== '() ty) (== '() datum))
      ((fresh (a d ta td)
         (== `(,a . ,d) datum)
         (== `(cons ,ta ,td) ty)
         (:-quoteo a ta)
         (:-quoteo d td)))
      ((:-literalo datum ty)))))

(define :-lambdao
  (lambda (x body env ty)
    (fresh (tyx tybody v)
      (== `(-> ,tyx ,tybody) ty)
      (:-expo body `((lambda (,x ,tyx . ,v)) . ,env) tybody))))

(define :-letrec-envo
  (lambda (b* env ty*)
    (conde
      ((== '() b*) (== '() ty*))
      ((fresh (p-name x body ty rb* rty*)
         (== `((,p-name . (lambda (,x) ,body)) . ,rb*) b*)
         (== `(,ty . ,rty*) ty*)
         (:-lambdao x body env ty)
         (:-letrec-envo rb* env rty*))))))

(define :-letreco
  (lambda (b* letrec-body env ty)
    (let loop ((b* b*) (rb* '()))
      (conde
        ((fresh (renv ty*)
           (== '() b*)
           (== `((letrec . ,rb*) . ,env) renv)
           (:-letrec-envo rb* renv ty*)
           (:-expo letrec-body renv ty)
           ))
        ((fresh (p-name x body b*-rest)
           (== `((,p-name (lambda (,x) ,body)) . ,b*-rest) b*)
           (symbolo p-name)
           (symbolo x)
           (loop b*-rest `((,p-name . (lambda (,x) ,body)) . ,rb*))))))))

(define :-leto
  (lambda (b* body env ty)
    (let loop ((b* b*) (rb* '()))
      (conde
        ((== '() b*) (:-expo body `((let . ,rb*) . ,env) ty))
        ((fresh (x e a tyx b*-rest)
           (== `((,x ,e) . ,b*-rest) b*)
           (symbolo x)
           (:-expo e env tyx)
           (loop b*-rest `((,x ,e . ,a) . ,rb*))))))))

(define eval-literalo
  (lambda (ty val)
    (conde
      ((== 'num ty) (numbero val))
      ((== 'bool ty) (== #t val))
      ((== 'bool ty) (== #f val)))))

(define eval-quoteo
  (lambda (ty val)
    (conde
      ((== 'sym ty) (symbolo val))
      ((== '() ty) (== '() val))
      ((fresh (a d ta td va vd)
         (== `(,a . ,d) val)
         (== `(cons ,ta ,td) ty)
         (eval-quoteo ta a)
         (eval-quoteo td d)))
      ((eval-literalo ty val)))))

(define eval-letreco
  (lambda (b* letrec-body env ty val)
    (let loop ((b* b*) (rb* '()))
      (conde
        ((== '() b*) (eval-expo letrec-body `((letrec . ,rb*) . ,env) ty val))
        ((fresh (p-name x body b*-rest)
           (== `((,p-name (lambda (,x) ,body)) . ,b*-rest) b*)
           (symbolo p-name)
           (symbolo x)
           (loop b*-rest `((,p-name . (lambda (,x) ,body)) . ,rb*))))))))

(define eval-leto
  (lambda (b* let-body env ty val)
    (let loop ((b* b*) (rb* '()))
      (conde
        ((== '() b*) (eval-expo let-body `((let . ,rb*) . ,env) ty val))
        ((fresh (x e tyx a b*-rest)
           (== `((,x ,e) . ,b*-rest) b*)
           (symbolo x)
           (eval-expo e env tyx a)
           (loop b*-rest `((,x . ,a) . ,rb*))))))))

(define not-in-env-letreco
  (lambda (rest x b*)
    (conde
      ((== '() b*) (not-in-envo x rest))
      ((fresh (b*-rest p-name rhs)
         (== `((,p-name . ,rhs) . ,b*-rest) b*)
         (=/= p-name x)
         (not-in-env-letreco rest x b*-rest))))))
(define not-in-env-leto
  (lambda (rest x b*)
    (conde
      ((== '() b*) (not-in-envo x rest))
      ((fresh (b*-rest p rhs)
         (== `((,p . ,rhs) . ,b*-rest) b*)
         (=/= p x)
         (not-in-env-leto rest x b*-rest))))))
(define not-in-envo
  (lambda (x env)
    (conde
      ((== '() env))
      ((fresh (rest y rhs)
         (== `((lambda (,y . ,rhs)) . ,rest) env)
         (=/= y x)
         (not-in-envo x rest)))
      ((fresh (rest b*)
         (== `((letrec . ,b*) . ,rest) env)
         (not-in-env-letreco rest x b*)))
      ((fresh (rest b*)
         (== `((let . ,b*) . ,rest) env)
         (not-in-env-leto rest x b*))))))

(define (lookup-letreco rest renv x b* ty val)
  (conde
    ((== '() b*) (lookupo x rest ty val))
    ((fresh (b*-rest p-name p body typ tybody)
       (== `((,p-name . (lambda (,p) ,body)) . ,b*-rest) b*)
       (conde
         ((fresh (vunknown)
            (== p-name x)
            (== `(,closure-tag ,p ,body ,renv) val)
            (== `(-> ,typ ,tybody) ty)
            (:-expo body `((lambda (,p ,typ . ,vunknown)) . ,renv) tybody)))
         ((=/= p-name x) (lookup-letreco rest renv x b*-rest ty val)))))))
(define (lookup-leto rest x b* ty val)
  (conde
    ((== '() b*) (lookupo x rest ty val))
    ((fresh (b*-rest y be bv)
       (== `((,y ,be . ,bv) . ,b*-rest) b*)
       (conde
         ((== y x) (== bv val) (:-expo be rest ty))
         ((=/= y x) (lookup-leto rest x b*-rest ty val)))))))
(define lookupo
  (lambda (x env ty val)
    (conde
      ((fresh (rest y bty bv)
         (== `((lambda (,y ,bty . ,bv)) . ,rest) env)
         (conde
           ((== y x) (== bty ty) (== bv val))
           ((=/= y x) (lookupo x rest ty val)))))
      ((fresh (rest b*)
         (== `((letrec . ,b*) . ,rest) env)
         (lookup-letreco rest env x b* ty val)))
      ((fresh (rest b*)
         (== `((let . ,b*) . ,rest) env)
         (lookup-leto rest x b* ty val))))))
