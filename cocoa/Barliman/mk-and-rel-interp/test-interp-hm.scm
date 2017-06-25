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
    (evalo '((lambda (id) id) (lambda (x) x)) type val))
  `((((-> _.0 _.0) (,closure-tag x x ())))))

(test 'eval-lambda-2
  (run* (type val)
    (evalo '((lambda (id) (id id)) (lambda (x) x)) type val))
  ;; Failure is expected because simulating let with lambda loses polymorphism.
  '())

(test 'eval-let-1
  (run* (type val)
    (evalo '(let ((id (lambda (x) x))) id) type val))
  `((((-> _.0 _.0) (,closure-tag x x ())))))

(test 'eval-let-2
  (run* (type val)
    (evalo '(let ((id (lambda (x) x))) (id id)) type val))
  `((((-> _.0 _.0) (,closure-tag x x ())))))

(test 'eval-letrec-1
  (run* (type val)
    (evalo '(letrec ((id (lambda (x) x))) id) type val))
  `((((-> _.0 _.0)
      (,closure-tag x x ((letrec-mono (id (-> _.1 _.1) lambda (x) x))))))))

(test 'eval-letrec-2
  (run* (type val)
    (evalo '(letrec ((id (lambda (x) x))) (id id)) type val))
  `((((-> _.0 _.0)
      (,closure-tag x x ((letrec-mono (id (-> _.1 _.1) lambda (x) x))))))))

(test 'eval-letrec-3
  (run* (type val)
    (evalo '(letrec ((id (lambda (x) x))) (lambda (y) (id y))) type val))
  `((((-> _.0 _.0)
      (,closure-tag y (id y) ((letrec-poly (id lambda (x) x))))))))

(test 'eval-letrec-4
  (run* (type val)
    (evalo '(letrec ((id (lambda (x) x))) (lambda (y) (id (id y)))) type val))
  `((((-> _.0 _.0)
      (,closure-tag y (id (id y)) ((letrec-poly (id lambda (x) x))))))))

(test 'eval-letrec-5
  (run* (type val)
    (evalo '(letrec ((id (lambda (x) x))) (lambda (y) ((id id) y))) type val))
  `((((-> _.0 _.0)
      (,closure-tag y ((id id) y) ((letrec-poly (id lambda (x) x))))))))

(test 'eval-letrec-6
  (run* (type val)
    (evalo '(letrec ((fix (lambda (f) (f (lambda (x) ((fix f) x))))))
              fix)
           type val))
  `((((-> (-> (-> _.0 _.1) (-> _.0 _.1)) (-> _.0 _.1))
      (,closure-tag f (f (lambda (x) ((fix f) x)))
                    ((letrec-mono
                       (fix (-> (-> (-> _.0 _.1) (-> _.0 _.1)) (-> _.0 _.1))
                            lambda (f) (f (lambda (x) ((fix f) x)))))))))))

(test 'eval-letrec-7
  (run* (type val)
    (evalo '(letrec ((fix (lambda (f) (f (lambda (x) ((fix f) x))))))
              (lambda (g) (fix g)))
           type val))
  `((((-> (-> (-> _.0 _.1) (-> _.0 _.1)) (-> _.0 _.1))
      (,closure-tag g (fix g)
                    ((letrec-poly
                       (fix lambda (f) (f (lambda (x) ((fix f) x)))))))))))

(test 'eval-letrec-8
  (run* (type val)
    (evalo '(letrec ((fix (lambda (f) (lambda (x) ((f (fix f)) x)))))
              fix)
           type val))
  `((((-> (-> (-> _.0 _.1) (-> _.0 _.1)) (-> _.0 _.1))
      (,closure-tag f (lambda (x) ((f (fix f)) x))
                    ((letrec-mono
                       (fix (-> (-> (-> _.0 _.1) (-> _.0 _.1)) (-> _.0 _.1))
                            lambda (f) (lambda (x) ((f (fix f)) x))))))))))

(test 'eval-letrec-9
  (run* (type val)
    (evalo '(letrec ((fix (lambda (f) (f (fix f)))))
              fix)
           type val))
  `((((-> (-> _.0 _.0) _.0)
      (,closure-tag f (f (fix f))
                    ((letrec-mono
                       (fix (-> (-> _.0 _.0) _.0)
                            lambda (f) (f (fix f))))))))))

(test 'eval-data-1
  (run* (type val) (evalo ''(1 . (x #t #f)) type val))
  '((((cons num (cons sym (cons bool (cons bool ())))) (1 x #t #f)))))

(test 'eval-data-2
  (run* (type val) (evalo '(cons 1 (cons 'x (cons #t (cons #f '()))))
                          type val))
  '((((cons num (cons sym (cons bool (cons bool ())))) (1 x #t #f)))))

(test 'eval-data-3
  (run* (type val) (evalo '(car (cons 1 (cons 'x (cons #t (cons #f '())))))
                          type val))
  '(((num 1))))

(test 'eval-data-4
  (run* (type val) (evalo '(cdr (cons 1 (cons 'x (cons #t (cons #f '())))))
                          type val))
  '((((cons sym (cons bool (cons bool ()))) (x #t #f)))))
