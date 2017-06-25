(load "mk/mk-vicare.scm")
(load "mk/mk.scm")
(load "mk/test-check.scm")
(load "interp-hm.scm")

(test 'lambda-1
  (run* (type)
    (:o '((lambda (id) id) (lambda (x) x)) type))
  '(((-> _.0 _.0))))

(test 'lambda-2
  (run* (type)
    (:o '((lambda (id) (id id)) (lambda (x) x)) type))
  ;; Failure is expected because simulating let with lambda loses polymorphism.
  '())

(test 'let-1
  (run* (type)
    (:o '(let ((id (lambda (x) x))) id) type))
  '(((-> _.0 _.0))))

(test 'let-2
  (run* (type)
    (:o '(let ((id (lambda (x) x))) (id id)) type))
  '(((-> _.0 _.0))))

(test 'letrec-1
  (run* (type)
    (:o '(letrec ((id (lambda (x) x))) id) type))
  '(((-> _.0 _.0))))

(test 'letrec-2
  (run* (type)
    (:o '(letrec ((id (lambda (x) x))) (id id)) type))
  '(((-> _.0 _.0))))

(test 'data-1
  (run* (type) (:o ''(1 . (x #t #f)) type))
  '(((cons num (cons sym (cons bool (cons bool ())))))))

(test 'data-2
  (run* (type) (:o '(cons 1 (cons 'x (cons #t (cons #f '())))) type))
  '(((cons num (cons sym (cons bool (cons bool ())))))))

(test 'data-3
  (run* (type) (:o '(car (cons 1 (cons 'x (cons #t (cons #f '()))))) type))
  '((num)))

(test 'data-4
  (run* (type) (:o '(cdr (cons 1 (cons 'x (cons #t (cons #f '()))))) type))
  '(((cons sym (cons bool (cons bool ()))))))
