(load "mk/mk-vicare.scm")
(load "mk/mk.scm")
(load "mk/test-check.scm")
(load "interp-hm.scm")

(test 'type-lambda-1
  (run* (type)
    (:o '((lambda (id) id) (lambda (x) x)) type))
  '(((-> _.0 _.0))))

(test 'type-lambda-2
  (run* (type)
    (:o '((lambda (id) (id id)) (lambda (x) x)) type))
  ;; Failure is expected because simulating let with lambda loses polymorphism.
  '())

(test 'type-let-1
  (run* (type)
    (:o '(let ((id (lambda (x) x))) id) type))
  '(((-> _.0 _.0))))

(test 'type-let-2
  (run* (type)
    (:o '(let ((id (lambda (x) x))) (id id)) type))
  '(((-> _.0 _.0))))

(test 'type-letrec-1
  (run* (type)
    (:o '(letrec ((id (lambda (x) x))) id) type))
  '(((-> _.0 _.0))))

(test 'type-letrec-2
  (run* (type)
    (:o '(letrec ((id (lambda (x) x))) (id id)) type))
  '(((-> _.0 _.0))))

(test 'type-letrec-3
  (run* (type)
    (:o '(letrec ((id (lambda (x) x))) (lambda (y) (id y))) type))
  '(((-> _.0 _.0))))

(test 'type-letrec-4
  (run* (type)
    (:o '(letrec ((id (lambda (x) x))) (lambda (y) (id (id y)))) type))
  '(((-> _.0 _.0))))

(test 'type-letrec-5
  (run* (type)
    (:o '(letrec ((id (lambda (x) x))) (lambda (y) ((id id) y))) type))
  '(((-> _.0 _.0))))

(test 'type-letrec-6
  (run* (type)
    (:o '(letrec ((fix (lambda (f) (f (lambda (x) ((fix f) x))))))
           fix)
        type))
  '(((-> (-> (-> _.0 _.1) (-> _.0 _.1)) (-> _.0 _.1)))))

(test 'type-letrec-7
  (run* (type)
    (:o '(letrec ((fix (lambda (f) (f (lambda (x) ((fix f) x))))))
           (lambda (g) (fix g)))
        type))
  '(((-> (-> (-> _.0 _.1) (-> _.0 _.1)) (-> _.0 _.1)))))

(test 'type-letrec-8
  (run* (type)
    (:o '(letrec ((fix (lambda (f) (lambda (x) ((f (fix f)) x)))))
           fix)
        type))
  '(((-> (-> (-> _.0 _.1) (-> _.0 _.1)) (-> _.0 _.1)))))

(test 'type-letrec-9
  (run* (type)
    (:o '(letrec ((fix (lambda (f) (f (fix f)))))
           fix)
        type))
  '(((-> (-> _.0 _.0) _.0))))

(test 'type-data-1
  (run* (type) (:o ''(1 . (x #t #f)) type))
  '(((cons num (cons sym (cons bool (cons bool ())))))))

(test 'type-data-2
  (run* (type) (:o '(cons 1 (cons 'x (cons #t (cons #f '())))) type))
  '(((cons num (cons sym (cons bool (cons bool ())))))))

(test 'type-data-3
  (run* (type) (:o '(car (cons 1 (cons 'x (cons #t (cons #f '()))))) type))
  '((num)))

(test 'type-data-4
  (run* (type) (:o '(cdr (cons 1 (cons 'x (cons #t (cons #f '()))))) type))
  '(((cons sym (cons bool (cons bool ()))))))


(test 'eval-lambda-1
  (run* (type val)
    (eval:o '((lambda (id) id) (lambda (x) x)) type val))
  `((((-> _.0 _.0) (,closure-tag x x ())))))

(test 'eval-lambda-2
  (run* (type val)
    (eval:o '((lambda (id) (id id)) (lambda (x) x)) type val))
  ;; Failure is expected because simulating let with lambda loses polymorphism.
  '())

(test 'eval-let-1
  (run* (type val)
    (eval:o '(let ((id (lambda (x) x))) id) type val))
  `((((-> _.0 _.0) (,closure-tag x x ())))))

(test 'eval-let-2
  (run* (type val)
    (eval:o '(let ((id (lambda (x) x))) (id id)) type val))
  `((((-> _.0 _.0) (,closure-tag x x ())))))

(test 'eval-letrec-1
  (run* (type val)
    (eval:o '(letrec ((id (lambda (x) x))) id) type val))
  `((((-> _.0 _.0)
      (,closure-tag x x ((letrec-mono (id (-> _.1 _.1) lambda (x) x))))))))

(test 'eval-letrec-2
  (run* (type val)
    (eval:o '(letrec ((id (lambda (x) x))) (id id)) type val))
  `((((-> _.0 _.0)
      (,closure-tag x x ((letrec-mono (id (-> _.1 _.1) lambda (x) x))))))))

(test 'eval-letrec-3
  (run* (type val)
    (eval:o '(letrec ((id (lambda (x) x))) (lambda (y) (id y))) type val))
  `((((-> _.0 _.0)
      (,closure-tag y (id y) ((letrec-poly (id lambda (x) x))))))))

(test 'eval-letrec-4
  (run* (type val)
    (eval:o '(letrec ((id (lambda (x) x))) (lambda (y) (id (id y)))) type val))
  `((((-> _.0 _.0)
      (,closure-tag y (id (id y)) ((letrec-poly (id lambda (x) x))))))))

(test 'eval-letrec-5
  (run* (type val)
    (eval:o '(letrec ((id (lambda (x) x))) (lambda (y) ((id id) y))) type val))
  `((((-> _.0 _.0)
      (,closure-tag y ((id id) y) ((letrec-poly (id lambda (x) x))))))))

(test 'eval-letrec-6
  (run* (type val)
    (eval:o '(letrec ((fix (lambda (f) (f (lambda (x) ((fix f) x))))))
              fix)
           type val))
  `((((-> (-> (-> _.0 _.1) (-> _.0 _.1)) (-> _.0 _.1))
      (,closure-tag f (f (lambda (x) ((fix f) x)))
                    ((letrec-mono
                       (fix (-> (-> (-> _.0 _.1) (-> _.0 _.1)) (-> _.0 _.1))
                            lambda (f) (f (lambda (x) ((fix f) x)))))))))))

(test 'eval-letrec-7
  (run* (type val)
    (eval:o '(letrec ((fix (lambda (f) (f (lambda (x) ((fix f) x))))))
              (lambda (g) (fix g)))
           type val))
  `((((-> (-> (-> _.0 _.1) (-> _.0 _.1)) (-> _.0 _.1))
      (,closure-tag g (fix g)
                    ((letrec-poly
                       (fix lambda (f) (f (lambda (x) ((fix f) x)))))))))))

(test 'eval-letrec-8
  (run* (type val)
    (eval:o '(letrec ((fix (lambda (f) (lambda (x) ((f (fix f)) x)))))
              fix)
           type val))
  `((((-> (-> (-> _.0 _.1) (-> _.0 _.1)) (-> _.0 _.1))
      (,closure-tag f (lambda (x) ((f (fix f)) x))
                    ((letrec-mono
                       (fix (-> (-> (-> _.0 _.1) (-> _.0 _.1)) (-> _.0 _.1))
                            lambda (f) (lambda (x) ((f (fix f)) x))))))))))

(test 'eval-letrec-9
  (run* (type val)
    (eval:o '(letrec ((fix (lambda (f) (f (fix f)))))
              fix)
           type val))
  `((((-> (-> _.0 _.0) _.0)
      (,closure-tag f (f (fix f))
                    ((letrec-mono
                       (fix (-> (-> _.0 _.0) _.0)
                            lambda (f) (f (fix f))))))))))

(test 'eval-data-1
  (run* (type val) (eval:o ''(1 . (x #t #f)) type val))
  '((((cons num (cons sym (cons bool (cons bool ())))) (1 x #t #f)))))

(test 'eval-data-2
  (run* (type val) (eval:o '(cons 1 (cons 'x (cons #t (cons #f '()))))
                          type val))
  '((((cons num (cons sym (cons bool (cons bool ())))) (1 x #t #f)))))

(test 'eval-data-3
  (run* (type val) (eval:o '(car (cons 1 (cons 'x (cons #t (cons #f '())))))
                          type val))
  '(((num 1))))

(test 'eval-data-4
  (run* (type val) (eval:o '(cdr (cons 1 (cons 'x (cons #t (cons #f '())))))
                          type val))
  '((((cons sym (cons bool (cons bool ()))) (x #t #f)))))

(time
  (test 'quine-1
    (run 1 (expr ty)
      (== '((lambda (x)
              (cons x (cons (cons 'quote (cons x '())) '())))
            '(lambda (x)
               (cons x (cons (cons 'quote (cons x '())) '()))))
          expr)
      (eval:o expr ty expr))
    '(((((lambda (x)
           (cons x (cons (cons 'quote (cons x '())) '())))
         '(lambda (x)
            (cons x (cons (cons 'quote (cons x '())) '()))))
        (cons
          (cons
            sym
            (cons
              (cons sym ())
              (cons
                (cons
                  sym
                  (cons
                    sym
                    (cons
                      (cons
                        sym
                        (cons
                          (cons
                            sym
                            (cons
                              (cons sym (cons sym ()))
                              (cons
                                (cons
                                  sym
                                  (cons sym (cons (cons sym (cons () ())) ())))
                                ())))
                          (cons (cons sym (cons () ())) ())))
                      ())))
                ())))
          (cons
            (cons
              sym
              (cons
                (cons
                  sym
                  (cons
                    (cons sym ())
                    (cons
                      (cons
                        sym
                        (cons
                          sym
                          (cons
                            (cons
                              sym
                              (cons
                                (cons
                                  sym
                                  (cons
                                    (cons sym (cons sym ()))
                                    (cons
                                      (cons
                                        sym
                                        (cons
                                          sym
                                          (cons (cons sym (cons () ())) ())))
                                      ())))
                                (cons (cons sym (cons () ())) ())))
                            ())))
                      ())))
                ()))
            ())))))))

;; This is too slow normally.  Disable let, letrec, car, and cdr for fast success.
;(time
  ;(test 'quine-2
    ;(run 1 (expr)
      ;(fresh (ty)
        ;(== '(cons
               ;(cons
                 ;sym
                 ;(cons
                   ;(cons sym ())
                   ;(cons
                     ;(cons
                       ;sym
                       ;(cons
                         ;sym
                         ;(cons
                           ;(cons
                             ;sym
                             ;(cons
                               ;(cons
                                 ;sym
                                 ;(cons
                                   ;(cons sym (cons sym ()))
                                   ;(cons
                                     ;(cons
                                       ;sym
                                       ;(cons sym (cons (cons sym (cons () ())) ())))
                                     ;())))
                               ;(cons (cons sym (cons () ())) ())))
                           ;())))
                     ;())))
               ;(cons
                 ;(cons
                   ;sym
                   ;(cons
                     ;(cons
                       ;sym
                       ;(cons
                         ;(cons sym ())
                         ;(cons
                           ;(cons
                             ;sym
                             ;(cons
                               ;sym
                               ;(cons
                                 ;(cons
                                   ;sym
                                   ;(cons
                                     ;(cons
                                       ;sym
                                       ;(cons
                                         ;(cons sym (cons sym ()))
                                         ;(cons
                                           ;(cons
                                             ;sym
                                             ;(cons
                                               ;sym
                                               ;(cons (cons sym (cons () ())) ())))
                                           ;())))
                                     ;(cons (cons sym (cons () ())) ())))
                                 ;())))
                           ;())))
                     ;()))
                 ;()))
            ;ty)
        ;(eval:o expr ty expr)))
    ;'((((lambda (_.0)
          ;(cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
        ;'(lambda (_.0)
           ;(cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
       ;(=/= ((_.0 cons)) ((_.0 quote)))
       ;(sym _.0)))))


;; Tests from: http://kanren.cvs.sourceforge.net/kanren/kanren/mini/type-inference.scm
;; Turn off letrec statements in :o to run these tests successfully.

;; Some examples from Djinn: inferring morphisms of the continuation monad.

;(define (cont a) `(-> (-> ,a r) r))

;; Deriving the type of return and call/cc in the continuation monad:
;;    Djinn> returnC ? a -> C a
;;    returnC :: a -> C a
;;    returnC x1 x2 = x2 x1

;(time
  ;(test 'cont-return
    ;(run 1 (q) (:o q `(-> a ,(cont 'a))))
    ;'(((lambda (_.0) (lambda (_.1) (_.1 _.0)))
       ;(=/= ((_.0 _.1)) ((_.0 lambda))) (sym _.0 _.1)))))

;;    Djinn> bindC ? C a -> (a -> C b) -> C b
;;    bindC :: C a -> (a -> C b) -> C b
;;    bindC x1 x2 x3 = x1 (\ c15 -> x2 c15 (\ c17 -> x3 c17))

;(time
  ;(test 'cont-bind
    ;(run 1 (q)
      ;(fresh (x1 x2 _) (== `(lambda (,x1) (lambda (,x2) ,_)) q)
        ;(:o q `(-> ,(cont 'a) (-> (-> a ,(cont 'b)) ,(cont 'b))))))
    ;'(((lambda (_.0)
         ;(lambda (_.1)
           ;(lambda (_.2) (_.0 (lambda (_.3) ((_.1 _.3) _.2))))))
       ;(=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 lambda)) ((_.1 _.2)) ((_.1 _.3))
            ;((_.1 lambda)) ((_.2 _.3)) ((_.2 lambda)))
       ;(sym _.0 _.1 _.2 _.3)))))

;;    Djinn> type C a = (a -> r) -> r
;;    Djinn> callCC ? ((a -> C b) -> C a) -> C a
;;    callCC x1 x2 = x1 (\ c15 _ -> x2 c15) (\ c11 -> x2 c11)

;(time
  ;(test 'cont-call/cc
    ;(run 1 (q)
      ;(fresh (x1 x2 _) (== `(lambda (,x1) (lambda (,x2) ,_)) q)
        ;(:o q `(-> (-> (-> a ,(cont 'b)) ,(cont 'a)) ,(cont 'a)))))
    ;'(((lambda (_.0)
         ;(lambda (_.1)
           ;((lambda (_.2) (_.2 _.1))
            ;(_.0 (lambda (_.3) (lambda (_.4) (_.1 _.3)))))))
       ;(=/= ((_.0 _.1)) ((_.0 lambda)) ((_.1 _.2)) ((_.1 _.3)) ((_.1 _.4))
            ;((_.1 lambda)) ((_.3 _.4)) ((_.3 lambda)))
       ;(sym _.0 _.1 _.2 _.3 _.4)))))


;;; Inferring the expressions for shift and reset

;(define (cont2 a r) `(-> (-> ,a ,r) ,r))

;;; reset

;(time
  ;(test 'cont2-reset
    ;(run 1 (q)
      ;(fresh (x1 x2 _) (== q `(lambda (,x1) (lambda (,x2) ,_)))
        ;(:o q `(-> ,(cont2 'a 'a) ,(cont2 'a 'r)))))
    ;'(((lambda (_.0)
         ;(lambda (_.1)
           ;(_.1 (_.0 (lambda (_.2) _.2)))))
       ;(=/= ((_.0 _.1)) ((_.0 lambda)) ((_.1 lambda)))
       ;(sym _.0 _.1 _.2)))))

;;; shift
;;; This one takes forever.  Limiting expressions to only variables, lambdas,
;;; and applications improves performance dramatically.

;(time
  ;(test 'cont2-shift
    ;(run 1 (q)
      ;(fresh (x1 x2 _) (== q `(lambda (,x1) (lambda (,x2) ,_)))
        ;(:o q `(-> (-> (-> a ,(cont2 'b 'r)) ,(cont2 'b 'b)) ,(cont2 'a 'b)))))
    ;'(((lambda (_.0)
         ;(lambda (_.1)
           ;((lambda (_.2)
              ;(_.2 (lambda (_.3) _.3)))
            ;(_.0 (lambda (_.4)
                   ;(lambda (_.5) (_.5 (_.1 _.4))))))))
       ;(=/= ((_.0 _.1)) ((_.0 lambda)) ((_.1 _.4)) ((_.1 _.5)) ((_.1 lambda))
            ;((_.2 lambda)) ((_.4 _.5)) ((_.4 lambda)))
       ;(sym _.0 _.1 _.2 _.3 _.4 _.5)))))
