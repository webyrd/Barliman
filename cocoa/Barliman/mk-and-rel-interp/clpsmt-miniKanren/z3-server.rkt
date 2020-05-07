#lang racket

(require racket/system)

(provide (all-defined-out))

(define z3-counter-check-sat 0)
(define z3-counter-get-model 0)

(define z3-out #f)
(define z3-in #f)
(define z3-err #f)
(define z3-p #f)
(define (z3-reset!)
  (let ((r
         (process "z3 -in")))
    (set! z3-in (car r))
    (set! z3-out (cadr r))
    (set! z3-p (caddr r))
    (set! z3-err (cadddr r))))
(z3-reset!)
(define (z3-check-in!)
  (if (eof-object? z3-in)
      (error 'z3-check-in "z3 input port")
      ;; (if (= 0 (mod z3-counter-check-sat 300))
      ;;     (z3-reset!)
      ;;     #t)
      #t))

(define read-sat
  (lambda ()
    (z3-check-in!)
    (let ([r (read z3-in)])
      (if (eq? r 'sat)
          #t
          (if (eq? r 'unsat)
              #f
              (if (eq? r 'unknown)
                  (begin
                    (printf "read-sat: unknown\n")
                    ;;(call-z3 '((pop)))
                    #f)
                  (error 'read-sat (format "~a" r))))))))

(define call-z3
  (lambda (xs)
    (for-each (lambda (x)
                ;;(printf "~a\n" x)
                (fprintf z3-out "~a\n" x)) xs)
    (flush-output z3-out)))

(define check-sat
  (lambda (xs)
    (call-z3 (append (cons '(reset) xs) '((check-sat))))
    (set! z3-counter-check-sat (+ z3-counter-check-sat 1))
    (read-sat)))

(define read-model
  (lambda ()
    (let ([m (read z3-in)])
      ;;(display m)
      (map (lambda (x)
             (cons (cadr x)
                   (if (null? (caddr x))
                       (let ([r (cadddr (cdr x))])
                         (cond
                           ((eq? r 'false) #f)
                           ((eq? r 'true) #t)
                           ((and (pair? (cadddr x)) (eq? (cadr (cadddr x)) 'BitVec)) r)
                           (else (eval r))))
                       `(lambda ,(map car (caddr x)) ,(cadddr (cdr x))))))
           (cdr m)))))

(define get-model-inc
  (lambda ()
    (call-z3 '((get-model)))
    (set! z3-counter-get-model (+ z3-counter-get-model 1))
    (read-model)))

(define get-model
  (lambda (xs)
    (and (check-sat xs)
         (get-model-inc))))

(define neg-model
  (lambda (model)
    (cons
     'assert
     (list
      (cons
       'or
       (map
        (lambda (xv)
          `(not (= ,(car xv) ,(cdr xv))))
        model))))))

(define get-next-model
  (lambda (xs ms)
    (let* ([ms (map (lambda (m)
                      (filter (lambda (x) ; ignoring functions
                                (or (number? (cdr x))
                                    (symbol? (cdr x)) ; for bitvectors
                                    )) m))
                    ms)])
      (if (member '() ms) #f  ; if we're skipping a model, let us stop
          (and (check-sat (append xs (map neg-model ms)))
               (get-model-inc))))))
