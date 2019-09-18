(define z3-counter-check-sat 0)
(define z3-counter-get-model 0)

(define read-sat
  (lambda (fn)
    (let ([p (open-input-file fn)])
      (let ([r (read p)])
        (close-input-port p)
        (eq? r 'sat)))))

(define call-z3
  (lambda (xs)
    (let ([p (open-output-file "out.smt" 'replace)])
      (for-each (lambda (x) (fprintf p "~a\n" x)) xs)
      (close-output-port p)
      (system "sed -i '' 's/#t/true/g' out.smt")
      (system "sed -i '' 's/#f/false/g' out.smt")
      (system "sed -i '' 's/bitvec-/#b/g' out.smt")
      (let ((r (system "z3 out.smt >out.txt")))
        (system "sed -i '' 's/#b/bitvec-/g' out.txt")
        (if (not (= r 0))
            (error 'call-z3 "error in z3 out.smt > out.txt"))))))

(define check-sat
  (lambda (xs)
    (call-z3 (append xs '((check-sat) (exit))))
    (set! z3-counter-check-sat (+ z3-counter-check-sat 1))
    (read-sat "out.txt")))

(define read-model
  (lambda (fn)
    (let ([p (open-input-file fn)])
      (let ([r (read p)])
        (if (eq? r 'sat)
            (let ([m (read p)])
              (close-input-port p)
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
                   (cdr m)))
            (begin
              (close-input-port p)
              #f))))))

(define get-model
  (lambda (xs)
    (call-z3 (append xs '((check-sat) (get-model) (exit))))
    (set! z3-counter-get-model (+ z3-counter-get-model 1))
    (read-model "out.txt")))

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

(define check-model-unique
  (lambda (xs model)
    (let ([r
           (check-sat
            (append xs (list (neg-model model))))])
      (not r))))

(define get-all-models
  (lambda (xs ms)
    (let* ([ys (append xs (map neg-model ms))])
      (if (not (check-sat ys))
          (reverse ms)
          (get-all-models xs (cons (get-model ys) ms))))))

(define get-next-model
  (lambda (xs ms)
    (let* ([ms (map (lambda (m)
                      (filter (lambda (x) ; ignoring functions
                                (or (number? (cdr x))
                                    (symbol? (cdr x)) ; for bitvectors
                                    )) m))
                    ms)])
      (if (member '() ms) #f  ; if we're skipping a model, let us stop
          (let ([ys (append xs (map neg-model ms))])
            (and (check-sat ys)
                 (get-model ys)))))))
