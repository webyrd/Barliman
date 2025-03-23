(load "chez-load-interp.scm")
(load "mk/test-check.scm")

;; Adapted from https://github.com/k-tsushima/Shin-Barliman/blob/master/transformations/peano.scm
;; and https://github.com/k-tsushima/Shin-Barliman/blob/master/transformations/peano-relational.scm

(time
  (test "peano-synth-fib-aps 3"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns ACC1 ACC2)
             (let ()
               (fresh (A B begin-body)
                 
                 ;; skeleton
                 (== `((define zero?
                         (lambda (n)
                           (equal? 'z n)))
                       (define add1
                         (lambda (n)
                           (cons 's n)))
                       (define sub1
                         (lambda (n)
                           (and (equal? (car n) 's)
                                (cdr n))))
                       (define +
                         (lambda (n m)
                           (if (zero? n)
                               m
                               (add1 (+ (sub1 n) m)))))
                       (define -
                         (lambda (n m)
                           (if (zero? m)
                               n
                               (sub1 (- n (sub1 m))))))
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               a1
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) ,A ,B))))))
                     defns)
		 
                 (appendo defns
                          `(((lambda x x)
                             (fib-aps 'z ',ACC1 ',ACC2)
                             (fib-aps '(s . z) ',ACC1 ',ACC2)
                             (fib-aps '(s s . z) ',ACC1 ',ACC2)
                             (fib-aps '(s s s . z) ',ACC1 ',ACC2)
                             (fib-aps '(s s s s . z) ',ACC1 ',ACC2)
                             (fib-aps '(s s s s s . z) ',ACC1 ',ACC2)))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        '(z
                          (s . z)
                          (s . z)
                          (s s . z)
                          (s s s . z)
                          (s s s s s . z)))))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))
       (ans-allTests))

     ;; result!
     '(((((define zero?
            (lambda (n)
              (equal? 'z n)))
          (define add1
            (lambda (n)
              (cons 's n)))
          (define sub1
            (lambda (n)
              (and (equal? (car n) 's)
                   (cdr n))))
          (define +
            (lambda (n m)
              (if (zero? n)
                  m
                  (add1 (+ (sub1 n) m)))))
          (define -
            (lambda (n m)
              (if (zero? m)
                  n
                  (sub1 (- n (sub1 m))))))
          (define fib-aps
            (lambda (n a1 a2)
              (if (zero? n)
                  a1
                  (if (zero? (sub1 n))
                      a2
                      (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))))
         z
         (s . z)))))
  
 )

;; Like the peano-synth-fib-aps 3 above, but we also synthesize one of the values for one of the base cases
(time
  (test "peano-synth-fib-aps 4"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns ACC1 ACC2)
             (let ()
               (fresh (BASE A B begin-body)

                 ;; skeleton
                 (== `((define zero?
                         (lambda (n)
                           (equal? 'z n)))
                       (define add1
                         (lambda (n)
                           (cons 's n)))
                       (define sub1
                         (lambda (n)
                           (and (equal? (car n) 's)
                                (cdr n))))
                       (define +
                         (lambda (n m)
                           (if (zero? n)
                               m
                               (add1 (+ (sub1 n) m)))))
                       (define -
                         (lambda (n m)
                           (if (zero? m)
                               n
                               (sub1 (- n (sub1 m))))))
                       (define fib-aps
                         (lambda (n a1 a2)
                           (if (zero? n)
                               ,BASE
                               (if (zero? (sub1 n))
                                   a2
                                   (fib-aps (- n '(s . z)) ,A ,B))))))
                     defns)

                 (appendo defns
                          `(((lambda x x)
                             (fib-aps 'z ',ACC1 ',ACC2)
                             (fib-aps '(s . z) ',ACC1 ',ACC2)
                             (fib-aps '(s s . z) ',ACC1 ',ACC2)
                             (fib-aps '(s s s . z) ',ACC1 ',ACC2)
                             (fib-aps '(s s s s . z) ',ACC1 ',ACC2)
                             (fib-aps '(s s s s s . z) ',ACC1 ',ACC2)))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        '(z
                          (s . z)
                          (s . z)
                          (s s . z)
                          (s s s . z)
                          (s s s s s . z)))))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))
       (ans-allTests))

     ;; result!
     '(((((define zero?
            (lambda (n)
              (equal? 'z n)))
          (define add1
            (lambda (n)
              (cons 's n)))
          (define sub1
            (lambda (n)
              (and (equal? (car n) 's)
                   (cdr n))))
          (define +
            (lambda (n m)
              (if (zero? n)
                  m
                  (add1 (+ (sub1 n) m)))))
          (define -
            (lambda (n m)
              (if (zero? m)
                  n
                  (sub1 (- n (sub1 m))))))
          (define fib-aps
            (lambda (n a1 a2)
              (if (zero? n)
                  a1
                  (if (zero? (sub1 n))
                      a2
                      (fib-aps (- n '(s . z)) a2 (+ a1 a2)))))))
         z
         (s . z)))))

 )
