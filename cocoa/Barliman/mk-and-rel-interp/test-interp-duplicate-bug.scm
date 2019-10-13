(load "chez-load-interp.scm")
(load "mk/smt.scm")
;;(load "clpsmt-miniKanren/z3-driver.scm")
(load "clpsmt-miniKanren/z3-server.scm")
(load "clpsmt-miniKanren/test-check.scm")
(load "mk/test-check.scm")

(define-syntax Barliman
  (syntax-rules ()
    [(_ (g ...) (A ...) def inputs outputs)
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns)
             (let ((g (gensym (symbol->string `,g))) ...)
               (fresh (A ... begin-body)
                 (fresh (defn-list)
                   
                   (== defns defn-list)
                   
                   (absento g defn-list) ...)

                 ;; skeleton
                 (== `((define !
                         ,`def))
                     defns)
		             
                 (appendo defns
                          `(((lambda x x)
                             ,@(map (lambda (x) `(! ,x)) inputs)))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        outputs)
                 
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))]))

(time-test
 "factorial-synthesis-4e"
 (Barliman
  () (A)
  (lambda (n) 
    (if (zero? n)
        1
        (* n ,A)))
  '(0 3 4)
  '(1 6 24))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

(time-test
 "factorial-synthesis-4e exact copy 1"
 (Barliman
  () (A)
  (lambda (n) 
    (if (zero? n)
        1
        (* n ,A)))
  '(0 3 4)
  '(1 6 24))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

(time-test
 "factorial-synthesis-4e exact copy 2"
 (Barliman
  () (A)
  (lambda (n) 
    (if (zero? n)
        1
        (* n ,A)))
  '(0 3 4)
  '(1 6 24))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))
