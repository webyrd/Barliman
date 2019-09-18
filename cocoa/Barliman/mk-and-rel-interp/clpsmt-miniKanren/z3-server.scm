(define z3-counter-check-sat 0)
(define z3-counter-get-model 0)

(define-values (z3-out z3-in z3-err z3-p)
  (open-process-ports "z3 -in" 'block (native-transcoder)))
(define (z3-reset!)
  (let-values (((out in err p)
                (open-process-ports "z3 -in" 'block (native-transcoder))))
    (set! z3-out out)
    (set! z3-in in)
    (set! z3-err err)
    (set! z3-p p)))
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
      (eq? r 'sat))))

(define call-z3
  (lambda (xs)
    (for-each (lambda (x) (fprintf z3-out "~a\n" x)) xs)
    (flush-output-port z3-out)))

(define check-sat
  (lambda (xs)
    (call-z3 (append (cons '(reset) xs) '((check-sat))))
    (set! z3-counter-check-sat (+ z3-counter-check-sat 1))
    (read-sat)))

(define read-model
  (lambda ()
    (let ([m (read z3-in)])
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
