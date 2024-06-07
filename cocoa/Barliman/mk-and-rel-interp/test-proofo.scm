(load "chez-load-interp.scm")
(load "mk/test-check.scm")

;; Adapted from https://github.com/k-tsushima/Shin-Barliman/blob/master/transformations/peano.scm
;; and https://github.com/k-tsushima/Shin-Barliman/blob/master/transformations/peano-relational.scm

(time
  (test "proofo 1"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (prf)
             (let ()
               (fresh
                (body)
                (== prf `(C (A (A => B) (B => C)) . ,body))
                (evalo
                 `(letrec ([member?
                            (lambda (x ls)
                              (if (null? ls) #f
                                  (if (equal? (car ls) x) #t
                                      (member? x (cdr ls)))))])
                    (letrec ([proof?
                              (lambda (proof)
                                (match proof
                                  [`(,A ,assms assumption ()) (member? A assms)]
                                  [`(,B ,assms modus-ponens
                                        (((,A => ,B) ,assms ,r1 ,ants1)
                                         (,A ,assms ,r2 ,ants2)))
                                   (and (proof? (list (list A '=> B) assms r1 ants1))
                                        (proof? (list A assms r2 ants2)))]
                                  [`((,A => ,B) ,assms conditional
                                     ((,B (,A . ,assms) ,rule ,ants)))
                                   (proof? (list B (cons A assms) rule ants))]
                                  ))])
                      (proof? ',prf)))
                 #t)))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))
       (ans-allTests))

     ;; result!
     '(((C (A (A => B) (B => C))
             modus-ponens
             (((B => C) (A (A => B) (B => C)) assumption ())
              (B (A (A => B) (B => C))
                 modus-ponens
                 (((A => B) (A (A => B) (B => C)) assumption ())
                  (A (A (A => B) (B => C)) assumption ()))))))))
 
 )
