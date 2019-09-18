(define cvc4-counter-check-sat 0)
(define cvc4-counter-get-model 0)

(define read-sat
  (lambda (fn)
    (let ([p (open-input-file fn)])
      (let ([r (read p)])
        (close-input-port p)
        (eq? r 'sat)))))

(define call-cvc4
  (lambda (xs)
    (let ([p (open-output-file "out.smt" 'replace)])
      (for-each (lambda (x) (fprintf p "~a\n" x))
                (cons '(set-logic ALL_SUPPORTED) xs))
      (close-output-port p)
      (system "sed -i '' 's/#t/true/g' out.smt")
      (system "sed -i '' 's/#f/false/g' out.smt")
      (system "sed -i '' 's/bitvec-/#b/g' out.smt")
      (let ((r (system "cvc4 -m --lang smt out.smt >out.txt")))
        (system "sed -i '' 's/#b/bitvec-/g' out.txt")
        (if (not (= r 0))
            (error 'call-cvc4 "error in cvc4 out.smt > out.txt"))))))

(define check-sat
  (lambda (xs)
    (call-cvc4 (append xs '((check-sat) (exit))))
    (set! cvc4-counter-check-sat (+ cvc4-counter-check-sat 1))
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
                                   ((number? r) r)
                                   (else r)))
                               `(lambda ,(map car (caddr x)) ,(cadddr (cdr x))))))
                   (filter (lambda (x) (not (declare-datatypes? x))) (cdr m))))
            (begin
              (close-input-port p)
              #f))))))

(define get-model
  (lambda (xs)
    (call-cvc4 (append xs '((check-sat) (get-model) (exit))))
    (set! cvc4-counter-get-model (+ cvc4-counter-get-model 1))
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
    (let ([ys (append xs (map neg-model ms))])
      (and (check-sat ys)
           (get-model ys)))))
