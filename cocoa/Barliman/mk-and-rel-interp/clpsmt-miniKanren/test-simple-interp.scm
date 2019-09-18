(load "simple-interp.scm")

(test "running backwards"
  (run 5 (q) (evalo q '(closure y x ((x . (closure z z ()))))))
  '(((lambda (x) (lambda (y) x)) (lambda (z) z))
    ((lambda (x) (x (lambda (y) x))) (lambda (z) z))
    (((lambda (x) (lambda (y) x))
      ((lambda (_.0) _.0) (lambda (z) z)))
     (sym _.0))
    (((lambda (_.0) _.0)
      ((lambda (x) (lambda (y) x)) (lambda (z) z)))
     (sym _.0))
    ((((lambda (_.0) _.0) (lambda (x) (lambda (y) x)))
      (lambda (z) z))
     (sym _.0))))

(define lookupo
  (lambda (x env t)
    (fresh (rest y v)
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== y x) (== v t))
        ((=/= y x) (lookupo x rest t))))))

(test "eval-exp-lc 1"
  (run* (q) (evalo '(((lambda (x) (lambda (y) x)) (lambda (z) z)) (lambda (a) a)) q))
  '((closure z z ())))

(test "eval-exp-lc 2"
  (run* (q) (evalo '((lambda (x) (lambda (y) x)) (lambda (z) z)) q))
  '((closure y x ((x . (closure z z ()))))))

(test "running backwards"
  (run 5 (q) (evalo q '(closure y x ((x . (closure z z ()))))))
  '(((lambda (x) (lambda (y) x)) (lambda (z) z))
    ((lambda (x) (x (lambda (y) x))) (lambda (z) z))
    (((lambda (x) (lambda (y) x))
      ((lambda (_.0) _.0) (lambda (z) z)))
     (sym _.0))
    ((((lambda (_.0) _.0) (lambda (x) (lambda (y) x)))
      (lambda (z) z))
     (sym _.0))
    (((lambda (_.0) _.0)
      ((lambda (x) (lambda (y) x)) (lambda (z) z)))
     (sym _.0))))

(test "fully-running-backwards"
  (run 5 (q)
    (fresh (e v)
      (evalo e v)
      (== `(,e ==> ,v) q)))
  '((((lambda (_.0) _.1)
      ==> (closure _.0 _.1 ())) (sym _.0))
    ((((lambda (_.0) _.0) (lambda (_.1) _.2))
      ==>
      (closure _.1 _.2 ()))
     (sym _.0 _.1))
    ((((lambda (_.0) (lambda (_.1) _.2)) (lambda (_.3) _.4))
      ==>
      (closure _.1 _.2 ((_.0 . (closure _.3 _.4 ())))))
     (=/= ((_.0 lambda)))
     (sym _.0 _.1 _.3))
    ((((lambda (_.0) (_.0 _.0)) (lambda (_.1) _.1))
      ==>
      (closure _.1 _.1 ()))
     (sym _.0 _.1))
    ((((lambda (_.0) (_.0 _.0))
       (lambda (_.1) (lambda (_.2) _.3)))
      ==>
      (closure _.2 _.3 ((_.1 . (closure _.1 (lambda (_.2) _.3) ())))))
     (=/= ((_.1 lambda)))
     (sym _.0 _.1 _.2))))
