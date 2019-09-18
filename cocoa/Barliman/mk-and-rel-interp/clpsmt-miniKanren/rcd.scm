(load "mk.scm")
(load "cvc4-driver.scm")
(load "test-check.scm")
;;(load "cvc4-set-tests.scm")
;;(load "clpset.scm")

(define (declare-datatypes)
  (fresh ()
    (z/ '(declare-datatypes
          ((Ty 0) (listTy 0))
          (((arr (arr_param Ty) (arr_return Ty))
            (rcd (rcd_set listTy)))
           ((cons (head Ty) (tail listTy)) (nil)))))))

(define typ
  (lambda (t)
    (z/ `(declare-const ,t Ty))))

(define tf
  (lambda (f)
    (z/ `(declare-const ,f ty))))

(define tfs
  (lambda (s)
    (z/ `(declare-const ,s listTy))))

(define sub
  (lambda (t1 t2)
    (conde
      ((z/== t1 t2))
      ((z/assert `(not (= ,t1 ,t2)))
       (conde
         ((fresh (ta1 tb1 ta2 tb2)
            (typ ta1) (typ tb1) (typ ta2) (typ tb2)
            (z/== `(arr ,ta1 ,tb1) t1)
            (z/== `(arr ,ta2 ,tb2) t2)
            (sub ta2 ta1)
            (sub tb1 tb2)))
         ((fresh (r1 r2)
            (tfs r1) (tfs r2)
            (z/== `(rcd ,r1) t1)
            (z/== `(rcd ,r2) t2)
            (sub-rcd r1 r2))))))))

(define sub-rcd
  (lambda (r1 r2)
    (conde
      ((z/assert `(not (= ,r1 ,r2)))
       (conde
         ((z/== r2 'nil))
         ((fresh (a2 d2 a1 d1)
            (z/== r2 `(cons ,a2 ,d2))
            (z/== r1 `(cons ,a1 ,d1))
            (z/== a1 a2)
            (sub-rcd d2 d1))))))))

(test "1"
  (run 10 (q)
    (declare-datatypes)
    (fresh (t1 t2)
      (typ t1) (typ t2)
      (== q (list t1 t2))
      (sub t1 t2)))
  '(((rcd nil) (rcd nil)) ((arr (rcd nil) (rcd nil)) (arr (rcd nil) (rcd nil)))
    ((rcd (cons (rcd nil) nil)) (rcd (cons (rcd nil) nil)))
    ((arr (rcd nil) (arr (rcd nil) (rcd nil)))
     (arr (rcd nil) (arr (rcd nil) (rcd nil))))
    ((arr (arr (rcd nil) (rcd nil)) (rcd nil))
     (arr (arr (rcd nil) (rcd nil)) (rcd nil)))
    ((rcd (cons (rcd nil) (cons (rcd nil) nil)))
     (rcd (cons (rcd nil) (cons (rcd nil) nil))))
    ((arr (rcd nil) (rcd (cons (rcd nil) nil)))
     (arr (rcd nil) (rcd (cons (rcd nil) nil))))
    ((arr (arr (rcd nil) (rcd nil)) (arr (rcd nil) (rcd nil)))
     (arr (arr (rcd nil) (rcd nil)) (arr (rcd nil) (rcd nil))))
    ((arr (rcd (cons (rcd nil) nil)) (rcd nil))
     (arr (rcd (cons (rcd nil) nil)) (rcd nil)))
    ((rcd (cons (arr (rcd nil) (rcd nil)) nil))
     (rcd (cons (arr (rcd nil) (rcd nil)) nil)))))
