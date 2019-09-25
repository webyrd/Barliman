(define-syntax Barliman
  (syntax-rules ()
    [(_ (g ...) (A ...) defns body ...)
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns)
             (let ((g (gensym (symbol->string 'g))) ...)
               (fresh (A ...)
                 (fresh (defn-list)
                   
                   (== defns defn-list)
                   
                   (absento g defn-list) ...)

                 body ...
                 
                 ))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))]))

(Barliman (g1 g2 g3 g4 g5) (A B C begin-body) defns
                           (== `((define member? ;; predicate: two base cases, plus recursion
                         (lambda (x l) ;; must recur on l, since x is an atom
                           (if (null? l)
                               ,A ;; predicate, so base cases should be #f/#t
                               (let ((a (car l)) ;; simulate pattern matching on l
                                     (d (cdr l)))
                                 (if ,C ;; haven't used 'x' or 'a'—both should appear!
                                     ,B ;; predicate, so base cases should be #f/#t
                                     (member? x d) ;; recur on d—gets closer to base case
                                     ))))))
                     defns)

                 ;; constraints/side-conditions
                 ;; assuming predicate base cases
                 (=/= A B)
                 (booleano A)
                 (booleano B)
                 
                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (member? ',g1 '())
                             (member? ',g1 '(,g1))                            
                             (member? ',g1 '(,g2))
                             (member? ',g1 '(,g2 ,g1 ,g3 ,g4))
                             (member? ',g1 '(,g1 ,g2))
                             (member? ',g1 '(,g2 ,g3 ,g4 ,g5))
                             
                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list                         
                         ;; example outputs
                         #f
                         #t
                         #f
                         #t
                         #t
                         #f                         
                         ))

                 )
