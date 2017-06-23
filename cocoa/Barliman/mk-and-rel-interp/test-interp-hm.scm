(load "mk/mk-vicare.scm")
(load "mk/mk.scm")
(load "mk/test-check.scm")
(load "interp-hm.scm")

(test 'lambda-1
  (run 1 (type)
    (:o '((lambda (id) id) (lambda (x) x)) type))
  '(((-> _.0 _.0))))

(test 'lambda-2
  (run 1 (type)
    (:o '((lambda (id) (id id)) (lambda (x) x)) type))
  ;; Failure is expected because simulating let with lambda loses polymorphism.
  '())

(test 'let-1
  (run 1 (type)
    (:o '(let ((id (lambda (x) x))) id) type))
  '(((-> _.0 _.0))))

(test 'let-2
  (run 1 (type)
    (:o '(let ((id (lambda (x) x))) (id id)) type))
  '(((-> _.0 _.0))))

(test 'letrec-1
  (run 1 (type)
    (:o '(letrec ((id (lambda (x) x))) id) type))
  '(((-> _.0 _.0))))

(test 'letrec-2
  (run 1 (type)
    (:o '(letrec ((id (lambda (x) x))) (id id)) type))
  '(((-> _.0 _.0))))
