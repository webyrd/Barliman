(load "chez-load-interp.scm")
(load "mk/smt.scm")
(load "clpsmt-miniKanren/z3-server.scm")
;;(load "clpsmt-miniKanren/cvc4-server.scm")
(load "clpsmt-miniKanren/test-check.scm")
(load "mk/test-check.scm")

(time-test  "eval 7"
  (run* (q) (evalo '7 q))
  '((7)))

(time-test "eval (+ 3 4)"
  (run* (q) (evalo '(+ 3 4) q))
  '((7)))

(time-test "eval (zero? 0)"
  (run* (q) (evalo '(zero? 0) q))
  '((#t)))

(time-test "eval (zero? 5)"
  (run* (q) (evalo '(zero? 5) q))
  '((#f)))

#;(time-test "eval (zero? (- (* 2 3) (+ 1 5)))"
    (run* (q) (evalo '(zero? (- (* 2 3) (+ 1 5))) q))
    '((#t)))

#;(time-test "eval (zero? (- (* 2 3) (+ 1 4)))"
    (run* (q) (evalo '(zero? (- (* 2 3) (+ 1 4))) q))
    '((#f)))

#;(time-test "eval (= 0 0)"
    (run* (q) (evalo '(= 0 0) q))
    '((#t)))

#;(time-test "eval (= 1 0)"
    (run* (q) (evalo '(= 1 0) q))
    '((#f)))

(time-test "eval ((lambda (x) x) 0)"
  (run* (q) (evalo '((lambda (x) x) 0) q))
  '((0)))

(time-test "eval (let ((x 0)) x)"
  (run* (q) (evalo '(let ((x 0)) x) q))
  '((0)))

(time-test "eval (let ((x 0)) (cons x x))"
  (run* (q) (evalo '(let ((x 0)) (cons x x)) q))
  '(((0 . 0))))

(time-test "eval ((lambda (x) (+ x 1)) 0)"
  (run* (q) (evalo '((lambda (x) (+ x 1)) 0) q))
  '((1)))

(time-test "eval (let ((x 5)) (+ x 3))"
  (run* (q) (evalo '(let ((x 5)) (+ x 3)) q))
  '((8)))

#;(time-test "eval ((lambda (x) (= x 0)) 0)"
        (run* (q) (evalo '((lambda (x) (= x 0)) 0) q))
        '((#t)))

#;(time-test "eval (let ((x 0)) (= x 0))"
        (run* (q) (evalo '(let ((x 0)) (= x 0)) q))
        '((#t)))

#;(time-test "eval (let ((x 5)) (- x 3))"
        (run* (q) (evalo '(let ((x 5)) (- x 3)) q))
        '((2)))

(time-test "eval (let ((x 5)) (sub1 x))"
  (run* (q) (evalo '(let ((x 5)) (sub1 x)) q))
  '((4)))

(time-test "eval (let ((x 5)) (sub1 (sub1 x)))"
  (run* (q) (evalo '(let ((x 5)) (sub1 (sub1 x))) q))
  '((3)))


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


(test "append-nova-1"
  (run* (defn)
    (letrec ((presento (lambda (t1 t2)
                         (conde
                           ((== t1 t2))
                           ((fresh (a d)
                              (=/= t1 t2)
                              (== (cons a d) t2)
                              (conde
                                ((presento t1 a))
                                ((absento t1 a)
                                 (presento t1 d)))))))))
      (fresh (A B C)
        (evalo `(letrec ((append
                          (lambda (l s)
                            (if (null? l)
                                s
                                (cons (car l)
                                      (append (cdr l) s))))))
                  (list (append '() '())
                        (append '(a) '(b))
                        (append '(c) '(d))
                        (append '(e f) '(g h))))
               '(()
                 (a b)
                 (c d)
                 (e f g h))))))
  '((_.0)))

(test "append-nova-2"
  (run* (defn)
    (letrec ((presento (lambda (t1 t2)
                         (conde
                           ((== t1 t2))
                           ((fresh (a d)
                              (=/= t1 t2)
                              (== (cons a d) t2)
                              (conde
                                ((presento t1 a))
                                ((absento t1 a)
                                 (presento t1 d)))))))))
      (fresh (A B C)
        (== `(lambda (l s)
               (if (null? l)
                   s
                   (cons (car l)
                         (append (cdr l) s))))
            defn)
        (evalo `(letrec ((append
                          ,defn))
                  (list (append '() '())
                        (append '(a) '(b))
                        (append '(c) '(d))
                        (append '(e f) '(g h))))
               '(()
                 (a b)
                 (c d)
                 (e f g h))))))
  '(((lambda (l s)
       (if (null? l)
           s
           (cons (car l)
                 (append (cdr l) s)))))))

#|
;;; why is this so slowwwwww??
(time-test "append-nova-4b"
  (run* (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (A B C D)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)

        (== `(lambda (l s)
               (if (null? l)
                   ,A
                   (cons (car l) (append (cdr l) s))))
            defn)
        (evalo `(letrec ((append
                          ,defn))
                  (list (append '() '())
                        (append '(,g1) '(,g2))
                        (append '(,g3 ,g4) '(,g5 ,g6))))
               `(()
                 (,g1 ,g2)
                 (,g3 ,g4 ,g5 ,g6)))
        )))
  '(((lambda (l s)
       (if (null? l)
           s
           (cons (car l)
                 (append (cdr l) s)))))))
|#

#|
;; why is this so slow??
(time-test "append-nova-3b"
  (run* (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (A B C D)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)

        ;(symbolo B)
        ;(== 's A)
        ;(symbolo A)
        (== `(cons (car l) ,B) C)
        
        (== `(lambda (l s)
               (if (null? l)
                   ,A
                   ((lambda (,B) ,C) (append (cdr l) s))))
            defn)
        (evalo `(letrec ((append
                          ,defn))
                  (list (append '() '())
                        (append '(,g1) '(,g2))
                        (append '(,g3 ,g4) '(,g5 ,g6))))
               `(()
                 (,g1 ,g2)
                 (,g3 ,g4 ,g5 ,g6)))
        )))
  '(((lambda (l s)
       (if (null? l)
           s
           ((lambda (_.0) (cons (car l) _.0))
            (append (cdr l) s))))
     (=/= ((_.0 car)) ((_.0 cons)) ((_.0 l)))
     (sym _.0))))
|#

#|
;; why is this so slow???
(time-test "append-nova-3a"
  (run* (defn)
    (fresh (A B C D)
      (absento 'a defn)
      (absento 'b defn)
      (absento 'c defn)
      (absento 'd defn)
      (symbolo B)
      ;(== 's A)
      (symbolo A)
      (== `(cons (car l) ,B) C)
      (== `(lambda (l s)
             (if (null? l)
                 ,A
                 ((lambda (,B) ,C) (append (cdr l) s))))
          defn)
      (evalo `(letrec ((append
                        ,defn))
                (list (append '() '())
                      (append '(a) '(b))
                      (append '(c) '(d))
                      (append '(e f) '(g h))))
             '(()
               (a b)
               (c d)
               (e f g h)))
      ))
  '(((lambda (l s)
       (if (null? l)
           s
           ((lambda (_.0) (cons (car l) _.0))
            (append (cdr l) s))))
     (=/= ((_.0 car)) ((_.0 cons)) ((_.0 l)))
     (sym _.0))))
|#

#|
;; Why is this so slow?
(time-test "append-nova-3b"
  (run* (defn)
    (letrec ((presento (lambda (t1 t2)
                         (conde
                           ((== t1 t2))
                           ((fresh (a d)
                              (=/= t1 t2)
                              (== (cons a d) t2)
                              (conde
                                ((presento t1 a))
                                ((absento t1 a)
                                 (presento t1 d)))))))))
      (fresh (A B C D)
        ;(symbolo B)
        ;(== 's A)
        (symbolo A)
        (== `(cons (car l) ,B) C)
        (== `(lambda (l s)
               (if (null? l)
                   ,A
                   ((lambda (,B) ,C) (append (cdr l) s))))
            defn)
        (evalo `(letrec ((append
                          ,defn))
                  (list (append '() '())
                        (append '(a) '(b))
                        (append '(c) '(d))
                        (append '(e f) '(g h))))
               '(()
                 (a b)
                 (c d)
                 (e f g h)))
        )))
  '(((lambda (l s)
       (if (null? l)
           s
           ((lambda (_.0) (cons (car l) _.0))
            (append (cdr l) s))))
     (=/= ((_.0 car)) ((_.0 cons)) ((_.0 l)))
     (sym _.0))))
|#


(time-test
 "factorial-fully-ground"
 (Barliman
  () ()
  (lambda (n) 
    (if (zero? n)
        1
        (* n (! (sub1 n)))))
  '(0 1 2 3 5)
  '(1 1 2 6 120))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

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
 "factorial-synthesis-nova-4f"
 (Barliman
  () (A)
  (lambda (n)
    (if (zero? n)
        1
        (* n ,A)))
  '(0 1 3 4)
  '(1 1 6 24))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

(time-test
 "factorial-synthesis-nova-4g"
 (Barliman
  () (A B)
  (lambda (n)
    (if (zero? n)
        1
        (* ,A ,B)))
  '(0 1 3 4)
  '(1 1 6 24))
 '((((define ! (lambda (n) (if (zero? n) 1 (* (! (sub1 n)) n))))))))

(time-test
 "factorial-synthesis-nova-4g2"
 (Barliman
  () (A B)
  (lambda (n)
    (if (zero? n)
        1
        (* ,A ,B)))
  '(0 1 2 3 4)
  '(1 1 2 6 24))
 '((((define ! (lambda (n) (if (zero? n) 1 (* (! (sub1 n)) n))))))))

#|  ;; Slooww
(time-test
 "factorial-synthesis-nova-4g3"
 (Barliman
  () (A B)
  (lambda (n)
    (if (zero? n)
        1
        (* ,A ,B)))
  '(0 2 3 4)
  '(1 2 6 24))
 '((((define ! (lambda (n) (if (zero? n) 1 (* (! (sub1 n)) n))))))))
|#

(time-test
 "factorial-synthesis-4d"
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
 "factorial-synthesis-0"
 (Barliman
  () (A)
  (lambda (n)
    (if (zero? n)
        1
        (* ,A (! (sub1 n)))))
  '(0 1 2 3 5)
  '(1 1 2 6 120))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

(time-test
 "factorial-synthesis-0b"
 (Barliman
  () (A)
  (lambda (n)
    (if (zero? n)
        1
        (* n (! (sub1 ,A)))))
  '(0 1 2 3 5)
  '(1 1 2 6 120))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

(time-test
 "factorial-synthesis-1"
 (Barliman
  () (A)
  (lambda (n) 
    (if (zero? n)
        ,A
        (* n (! (sub1 n)))))
  '(0 1 2 3 5)
  '(1 1 2 6 120))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

(time-test
 "factorial-synthesis-2"
  (Barliman
  () (A)
  (lambda (n) 
    (if ,A
        1
        (* n (! (sub1 n)))))
  '(0 1 2 3 5)
  '(1 1 2 6 120))
  '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

(time-test
 "factorial-synthesis-2"
  (Barliman
   () (A)
   (lambda (n) 
     (if (zero? ,A)
         1
         (* n (! (sub1 n)))))
   '(0 5)
   '(1 120))
  '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

(time-test
 "factorial-synthesis-3b"
 (Barliman
   () (A)
   (lambda (n) 
     (if ,A
         1
         (* n (! (sub1 n)))))
   '(0 5)
   '(1 120))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

(time-test
 "factorial-synthesis-3"
  (Barliman
   () (A)
   (lambda (n) 
     (if (zero? ,A)
         1
         (* n (! (sub1 n)))))
  '(0 1 2 3 5)
  '(1 1 2 6 120))
  '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

(time-test
 "factorial-synthesis-4c"
 (Barliman
   () (A)
   (lambda (n) 
     (if (zero? n)
         1
         (* n (! ,A))))
   '(0 5)
   '(1 120))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

(time-test "synthesize shuffle with a skeleton, 1 hole, partial recursion"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))

                 ;; skeleton
                 (== `((define shuffle
                         (lambda (l1 l2) 
                           (if (null? l1)
                               '()
                               (let ((a1 (car l1))
                                     (d1 (cdr l1)))
                                 (if (null? l2) 
                                     '()
				     (let ((a2 (car l2)) 
					   (d2 (cdr l2)))
				       ,C)
                                     ))))))
                     defns)

                 ;; constraints/side-conditions
		 (conde
		  [(== `(shuffle d1 d2) C)]
		  [(fresh (f args)
			  (== `(let ((v (shuffle d1 d2))) (,f . ,args))
			      C)
			  (symbolo f))]
		  
		  [(fresh (e e* args)
			  (== `(let ((v (shuffle d1 d2))) ((,e . ,e*). ,args))
			      C))] 
		  
		  )

		 
                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (shuffle '() '())
                             (shuffle '() '(a))
			     (shuffle '(a) '())
			     (shuffle '(a) '(b))
			     (shuffle '(cat) '(dog))
			     (shuffle '(a c) '(b d))
			     (shuffle '(cat neko) '(fox kitsune))
			     (shuffle '(a c e) '(b d f))
                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list                         
                         ;; example outputs
                         '()
                         '()
			 '()
                         '(a b)
			 '(cat dog )
                         '(a b c d)
			 '(cat fox neko kitsune)
			 '(a b c d e f)
                         ))))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define shuffle (lambda (l1 l2) (if (null? l1) '() (let ((a1 (car l1)) (d1 (cdr l1))) (if (null? l2) '() (let ((a2 (car l2)) (d2 (cdr l2))) (cons a1 (cons a2 (shuffle d1 d2)))))))))))))


(time-test "synthesize shuffle with a skeleton, 1 hole"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))

                 ;; skeleton
                 (== `((define shuffle
                         (lambda (l1 l2) 
                           (if (null? l1)
                               '()
                               (let ((a1 (car l1))
                                     (d1 (cdr l1)))
                                 (if (null? l2) 
                                     '()
				     (let ((a2 (car l2)) 
					   (d2 (cdr l2)))
				       ,C)
                                     ))))))
                     defns)

                 ;; constraints/side-conditions
                                  
                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (shuffle '() '())
                             (shuffle '() '(a))
			     (shuffle '(a) '())
			     (shuffle '(a) '(b))
			     (shuffle '(cat) '(dog))
			     (shuffle '(a c) '(b d))
			     (shuffle '(cat neko) '(fox kitsune))
			     (shuffle '(a c e) '(b d f))
                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list                         
                         ;; example outputs
                         '()
                         '()
			 '()
                         '(a b)
			 '(cat dog )
                         '(a b c d)
			 '(cat fox neko kitsune)
			 '(a b c d e f)
                         ))))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define shuffle (lambda (l1 l2) (if (null? l1) '() (let ((a1 (car l1)) (d1 (cdr l1))) (if (null? l2) '() (let ((a2 (car l2)) (d2 (cdr l2))) (cons a1 (cons a2 (shuffle d1 d2)))))))))))))

(time-test "synthesize shuffle with a skeleton"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))

                 ;; skeleton
                 (== `((define shuffle
                         (lambda (l1 l2) 
                           (if (null? l1)
                               ,A 
                               (let ((a1 (car l1))
                                     (d1 (cdr l1)))
                                 (if (null? l2) 
                                     ,B 
				     (let ((a2 (car l2)) 
					   (d2 (cdr l2)))
				       ,C)
                                     ))))))
                     defns)

                 ;; constraints/side-conditions
                                  
                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (shuffle '() '())
                             (shuffle '() '(a))
			     (shuffle '(a) '())
			     (shuffle '(a) '(b))
			     (shuffle '(cat) '(dog))
			     (shuffle '(a c) '(b d))
			     (shuffle '(cat neko) '(fox kitsune))
			     (shuffle '(a c e) '(b d f))
                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list                         
                         ;; example outputs
                         '()
                         '()
			 '()
                         '(a b)
			 '(cat dog )
                         '(a b c d)
			 '(cat fox neko kitsune)
			 '(a b c d e f)
                         ))))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define shuffle (lambda (l1 l2) (if (null? l1) l1 (let ((a1 (car l1)) (d1 (cdr l1))) (if (null? l2) l2 (let ((a2 (car l2)) (d2 (cdr l2))) (cons a1 (cons a2 (shuffle d1 d2)))))))))))))

(time-test "synthesize member? with a skeleton, if-test hole"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))

                 ;; skeleton
                 (== `((define member? ;; predicate: two base cases, plus recursion
                         (lambda (x l) ;; must recur on l, since x is an atom
                           (if (null? l)
                               #f ;; predicate, so base cases should be #f/#t
                               (let ((a (car l)) ;; simulate pattern matching on l
                                     (d (cdr l)))
                                 (if ,B ;; haven't used 'x' or 'a'—both should appear!
                                     #t ;; predicate, so base cases should be #f/#t
                                     (member? x d) ;; recur on d—gets closer to base case
                                     ))))))
                     defns)

                 ;; constraints/side-conditions
                                  
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
                         ))))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define member?
           (lambda (x l)
             (if (null? l)
                 #f
                 (let ((a (car l))
                       (d (cdr l)))
                   (if (equal? a x)
                       #t
                       (member? x d))))))))))

(time-test "synthesize member? with a skeleton, if-test hole with 'a' and 'x' as fixed arguments"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))

                 ;; skeleton
                 (== `((define member? ;; predicate: two base cases, plus recursion
                         (lambda (x l) ;; must recur on l, since x is an atom
                           (if (null? l)
                               #f ;; predicate, so base cases should be #f/#t
                               (let ((a (car l)) ;; simulate pattern matching on l
                                     (d (cdr l)))
                                 (if ,B ;; haven't used 'x' or 'a'—both should appear!
                                     #t ;; predicate, so base cases should be #f/#t
                                     (member? x d) ;; recur on d—gets closer to base case
                                     ))))))
                     defns)

                 ;; constraints/side-conditions
                 ;; aggressive:
                 (== `(,C a x) B)

                 ;; if aggressive but wrong...
                 ;; (== `(,C a l) B)
                 ;; ... better hope the system can eventually invent a solution like:
                 ;; (== `(lambda (a ignore) (equal? ,D ,E)) C)

                 ;; alternative approach, less constrained:
                 ;; (== `(,C ,D ,E) B)
                 ;; (symbolo D)
                 ;; (symbolo E)
                                  
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
                         ))))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define member?
           (lambda (x l)
             (if (null? l)
                 #f
                 (let ((a (car l))
                       (d (cdr l)))
                   (if (equal? a x)
                       #t
                       (member? x d))))))))))

(time-test "synthesize member? with a skeleton, if-test hole, boolean holes with constraints"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))

                 ;; skeleton
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
                         ))))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define member?
           (lambda (x l)
             (if (null? l)
                 #f
                 (let ((a (car l))
                       (d (cdr l)))
                   (if (equal? a x)
                       #t
                       (member? x d))))))))))

;; this one takes a very long time if we remove type information from the boolean holes
(time-test "synthesize member? with a skeleton, if-test hole, boolean holes no constraints"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))

                 ;; skeleton
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
                 ;;(== #f A)
                 ; hint is necessary to make synthesis happen in a reasonable time!
                 (== #t B)

                 
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
                         ))))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define member?
           (lambda (x l)
             (if (null? l)
                 '#f
                 (let ((a (car l))
                       (d (cdr l)))
                   (if (equal? a x)
                       #t
                       (member? x d))))))))))

(time-test "synthesize member? with a skeleton, 2 holes, dan-member?"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))

                 ;; skeleton
                 (== `((define dan-member?
                         (lambda (x l)
                           (if (null? l)
                               #f
                               (if (equal? (car l) x)
                                   #t
                                   (dan-member? x (cdr l))))))
                       (define member? ;; predicate: two base cases, plus recursion
                         (lambda (x l) ;; must recur on l, since x is an atom
                           (if ,A
                               #f ;; predicate, so base cases should be #f/#t
                               (let ((a (car l)) ;; simulate pattern matching on l
                                     (d (cdr l)))
                                 (if ,B ;; haven't used 'x' or 'a'—both should appear!
                                     #t ;; predicate, so base cases should be #f/#t
                                     (dan-member? x d) ;; recur on d—gets closer to base case
                                     ))))))
                     defns)

                 ;; constraints/side-conditions
                 (absento 'dan-member? A)
                 (absento 'dan-member? B)
                 
                 
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
                         ))))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define dan-member?
           (lambda (x l)
             (if (null? l)
                 #f
                 (if (equal? (car l) x)
                     #t
                     (dan-member? x (cdr l))))))
         (define member?
           (lambda (x l)
             (if (null? l)
                 #f
                 (let ((a (car l))
                       (d (cdr l)))
                   (if (equal? a x)
                       #t
                       (dan-member? x d))))))))))

(time-test "synthesize member? with a skeleton, 2 holes"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))

                 ;; skeleton
                 (== `((define dan-member?
                         (lambda (x l)
                           (if (null? l)
                               #f
                               (if (equal? (car l) x)
                                   #t
                                   (dan-member? x (cdr l))))))
                       (define member? ;; predicate: two base cases, plus recursion
                         (lambda (x l) ;; must recur on l, since x is an atom
                           (if ,A
                               #f ;; predicate, so base cases should be #f/#t
                               (let ((a (car l)) ;; simulate pattern matching on l
                                     (d (cdr l)))
                                 (if ,B ;; haven't used 'x' or 'a'—both should appear!
                                     #t ;; predicate, so base cases should be #f/#t
                                     (member? x d) ;; recur on d—gets closer to base case
                                     ))))))
                     defns)

                 ;; constraints/side-conditions
                 (absento 'dan-member? A)
                 (absento 'dan-member? B)
                 
                 
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
                         ))))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define dan-member?
           (lambda (x l)
             (if (null? l)
                 #f
                 (if (equal? (car l) x)
                     #t
                     (dan-member? x (cdr l))))))
         (define member?
           (lambda (x l)
             (if (null? l)
                 #f
                 (let ((a (car l))
                       (d (cdr l)))
                   (if (equal? a x)
                       #t
                       (member? x d))))))))))

(time-test "synthesize all of append, using actual Barliman code, with a skeleton"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)
               
                   (== defns defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))

                 ;; skeleton
                 (== `((define append
                         (lambda (l s)
                           (if (null? ,A)
                               ,B
                               ,C))))
                     defns)

                 ;; constraint/side-condition to help prune search space
                 (symbolo A)
                 
                 
                 (appendo defns
                          `(((lambda x x)

                             ;; example inputs
                             (append '() '())
                             (append '(,g1) '(,g2))
                             (append '(,g3 ,g4) '(,g5 ,g6))

                             ))
                          begin-body)
                 (evalo `(begin . ,begin-body)
                        (list
                         
                         ;; example outputs
                         '()
                         `(,g1 ,g2)
                         `(,g3 ,g4 ,g5 ,g6)
                         
                         ))))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     ;; result!
     '((((define append
           (lambda (l s)
             (if (null? l)
                 s
                 (cons (car l)
                       (append (cdr l) s)))))))))

(time-test "synthesize all of append, using actual Barliman code, with a single variable representing the entire program"
     (let ()
       (define (ans-allTests)
         (define (results)
           (run 1 (defns)
             (let ((g1 (gensym "g1"))
                   (g2 (gensym "g2"))
                   (g3 (gensym "g3"))
                   (g4 (gensym "g4"))
                   (g5 (gensym "g5"))
                   (g6 (gensym "g6"))
                   (g7 (gensym "g7"))
                   (g8 (gensym "g8"))
                   (g9 (gensym "g9"))
                   (g10 (gensym "g10"))
                   (g11 (gensym "g11"))
                   (g12 (gensym "g12"))
                   (g13 (gensym "g13"))
                   (g14 (gensym "g14"))
                   (g15 (gensym "g15"))
                   (g16 (gensym "g16"))
                   (g17 (gensym "g17"))
                   (g18 (gensym "g18"))
                   (g19 (gensym "g19"))
                   (g20 (gensym "g20")))
               (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
                 (fresh (defn-list)

               
                   (== `( ,A ) defn-list)
               
                   (absento g1 defn-list)
                   (absento g2 defn-list)
                   (absento g3 defn-list)
                   (absento g4 defn-list)
                   (absento g4 defn-list)
                   (absento g5 defn-list)
                   (absento g6 defn-list)
                   (absento g7 defn-list)
                   (absento g8 defn-list)
                   (absento g9 defn-list)
                   (absento g10 defn-list)
                   (absento g11 defn-list)
                   (absento g12 defn-list)
                   (absento g13 defn-list)
                   (absento g14 defn-list)
                   (absento g15 defn-list)
                   (absento g16 defn-list)
                   (absento g17 defn-list)
                   (absento g18 defn-list)
                   (absento g19 defn-list)
                   (absento g20 defn-list))
             
                 (== `(,A) defns) (appendo defns `(((lambda x x) (append '() '()) (append '(,g1) '(,g2)) (append '(,g3 ,g4) '(,g5 ,g6))    )) begin-body) (evalo `(begin . ,begin-body) (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6)    ))))))
         (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
           (if (null? results-fast)
               (begin (set! allow-incomplete-search? #f) (results))
               results-fast)))

       (ans-allTests))

     '((((define append
           (lambda (_.0 _.1)
             (if (null? _.0)
                 _.1
                 (cons (car _.0) (append (cdr _.0) _.1))))))
        (=/= ((_.0 _.1)) ((_.0 append)) ((_.0 car)) ((_.0 cdr))
             ((_.0 cons)) ((_.0 if)) ((_.0 null?)) ((_.1 append))
             ((_.1 car)) ((_.1 cdr)) ((_.1 cons)) ((_.1 if))
             ((_.1 null?)))
        (sym _.0 _.1))))

;; synthesize all of append
(let ()
  (define (ans-allTests)
    (define (results)
      (run 1 (defns)
        (let ((g1 (gensym "g1"))
              (g2 (gensym "g2"))
              (g3 (gensym "g3"))
              (g4 (gensym "g4"))
              (g5 (gensym "g5"))
              (g6 (gensym "g6"))
              (g7 (gensym "g7"))
              (g8 (gensym "g8"))
              (g9 (gensym "g9"))
              (g10 (gensym "g10"))
              (g11 (gensym "g11"))
              (g12 (gensym "g12"))
              (g13 (gensym "g13"))
              (g14 (gensym "g14"))
              (g15 (gensym "g15"))
              (g16 (gensym "g16"))
              (g17 (gensym "g17"))
              (g18 (gensym "g18"))
              (g19 (gensym "g19"))
              (g20 (gensym "g20")))
          (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z begin-body)
            (fresh (defn-list)

               
              (== `( (define ,A
                       (lambda ,B
                         ,C)) ) defn-list)
               
              (absento g1 defn-list)
              (absento g2 defn-list)
              (absento g3 defn-list)
              (absento g4 defn-list)
              (absento g4 defn-list)
              (absento g5 defn-list)
              (absento g6 defn-list)
              (absento g7 defn-list)
              (absento g8 defn-list)
              (absento g9 defn-list)
              (absento g10 defn-list)
              (absento g11 defn-list)
              (absento g12 defn-list)
              (absento g13 defn-list)
              (absento g14 defn-list)
              (absento g15 defn-list)
              (absento g16 defn-list)
              (absento g17 defn-list)
              (absento g18 defn-list)
              (absento g19 defn-list)
              (absento g20 defn-list))
             
            (== `((define ,A
                    (lambda ,B
                      ,C))) defns) (appendo defns `(((lambda x x) (append '() '()) (append '(,g1) '(,g2)) (append '(,g3 ,g4) '(,g5 ,g6))    )) begin-body) (evalo `(begin . ,begin-body) (list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6)    ))))))
    (let ((results-fast (begin (set! allow-incomplete-search? #t) (results))))
      (if (null? results-fast)
          (begin (set! allow-incomplete-search? #f) (results))
          results-fast)))

  (time (ans-allTests)))

#!eof

(time-test "append-gensym-full-synthesis"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6")))            
      (fresh (A B C)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (== `(define append
               (lambda (l s)
                 (if (null? l) s (cons (car l) (append (cdr l) s)))))
            defn)
        (evalo `(begin
                  ,defn
                  (cons (append '() '())
                        (cons (append '(,g1) '(,g2))
                              (cons (append '(,g3 ,g4) '(,g5 ,g6))
                                    '()))))
               `(()
                 (,g1 ,g2)
                 (,g3 ,g4 ,g5 ,g6))))))
  '(((define append
       (lambda (l s)
         (if (null? l) s (cons (car l) (append (cdr l) s))))))))


(time-test "append-gensym-full-synthesis"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6")))            
      (fresh (A B C)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (== `(define ,A
               (lambda ,B
                 ,C))
            defn)
        (evalo `(begin
                  ,defn
                  (cons (append '() '())
                        (cons (append '(,g1) '(,g2))
                              (cons (append '(,g3 ,g4) '(,g5 ,g6))
                                    '()))))
               `(()
                 (,g1 ,g2)
                 (,g3 ,g4 ,g5 ,g6))))))
  '(((define append
       (lambda (l s)
         (if (null? l) s (cons (car l) (append (cdr l) s))))))))



(test 'letrec-keyword-reference-1
  (run* (q) (evalo '(letrec ((quote (lambda x 5))) quote)
                   q))
  '(((closure (lambda x 5) ((letrec (rec quote lambda x 5)) (val cons prim . cons) (val car prim . car) (val cdr prim . cdr) (val null? prim . null?) (val symbol? prim . symbol?) (val not prim . not) (val equal? prim . equal?) (val list closure (lambda x x) ()))))))

(test 'letrec-keyword-reference-2
  (run* (q) (evalo '(letrec ((foo (lambda x 5))) quote)
                   q))
  '())

(test 'letrec-keyword-reference-3
  (run* (q) (evalo '(letrec ((quote (lambda x quote))) 6)
                   q))
  '((6)))

(test 'letrec-keyword-reference-4
  (run* (q) (evalo '(letrec ((bar (lambda (y) quote)) (quote (lambda x x))) 6)
                   q))
  '((6)))

(test 'letrec-keyword-reference-5
  (run* (q) (evalo '(letrec ((quote (lambda x x)) (bar (lambda (y) quote))) 6)
                   q))
  '((6)))

(printf "*** this next test will fail, since 'evalo' doesn't check the syntax of lambda bodies")
(test 'letrec-keyword-reference-6
  (run* (q) (evalo '(letrec ((foo (lambda x x)) (bar (lambda (y) quote))) 6)
                   q))
  '())

(test 'letrec-keyword-reference-7
  (run* (q) (evalo '(letrec ((foo (lambda x x)) (bar (lambda (y) quote))) (bar 6))
                   q))
  '())




(test 'begin-keyword-reference-1
  (run* (q) (evalo '(begin (define quote (lambda x 5)) quote)
                   q))
  '(((closure (lambda x 5) ((letrec (rec quote lambda x 5)) (val cons prim . cons) (val car prim . car) (val cdr prim . cdr) (val null? prim . null?) (val symbol? prim . symbol?) (val not prim . not) (val equal? prim . equal?) (val list closure (lambda x x) ()))))))

(test 'begin-keyword-reference-2
  (run* (q) (evalo '(begin (define foo (lambda x 5)) quote)
                   q))
  '())

(test 'begin-keyword-reference-3
  (run* (q) (evalo '(begin (define quote (lambda x quote)) 6)
                   q))
  '((6)))

(test 'begin-keyword-reference-4
  (run* (q) (evalo '(begin (define bar (lambda (y) quote)) (define quote (lambda x x)) 6)
                   q))
  '((6)))

(test 'begin-keyword-reference-5
  (run* (q) (evalo '(begin (define quote (lambda x x)) (define bar (lambda (y) quote)) 6)
                   q))
  '((6)))

(printf "*** this next test will fail, since 'evalo' doesn't check the syntax of lambda bodies")
(test 'begin-keyword-reference-6
  (run* (q) (evalo '(begin (define foo (lambda x x)) (define bar (lambda (y) quote)) 6)
                   q))
  '())

(test 'begin-keyword-reference-7
  (run* (q) (evalo '(begin (define foo (lambda x x)) (define bar (lambda (y) quote)) (bar 6))
                   q))
  '())



(test 'letrec-with-no-bindings-1
  (run* (q)
    (evalo '(letrec ()
              5)
           q))
  '((5)))

(test 'begin-with-definitions-1
  (run* (q)
    (evalo '(begin 5) q))
  '((5)))

(test 'simple-begin-1
  (run* (q)
    (evalo '(begin
              (define member? (lambda x 6))
              5)
           q))
  '((5)))

(test 'simple-letrec-1
  (run* (q)
    (evalo '(letrec ((member? (lambda x 6)))
              5)
           q))
  '((5)))

(test 'append-1
  (run* (q)
    (evalo '(letrec ((append (lambda (l s)
                               (if (null? l)
                                   s
                                   (cons (car l)
                                         (append (cdr l) s))))))
              (append '(1 2 3) '(4 5)))
           q))
  '(((1 2 3 4 5))))

(test 'even?/odd?-1
  (run* (q)
    (evalo '(letrec ((even? (lambda (n)
                              (if (equal? 'z n)
                                  #t
                                  (odd? (cdr n)))))
                     (odd? (lambda (n)
                             (if (equal? 'z n)
                                 #f
                                 (even? (cdr n))))))
              (even? 'z))
           q))
  '((#t)))

(test 'even?/odd?-2
  (run* (q) (evalo '(letrec ((even? (lambda (n)
                                      (if (equal? 'z n)
                                          #t
                                          (odd? (cdr n)))))
                             (odd? (lambda (n)
                                     (if (equal? 'z n)
                                         #f
                                         (even? (cdr n))))))
                      (odd? 'z))
                   q))
  '((#f)))

(test 'even?/odd?-3
  (run* (q) (evalo '(letrec ((even? (lambda (n)
                                      (if (equal? 'z n)
                                          #t
                                          (odd? (cdr n)))))
                             (odd? (lambda (n)
                                     (if (equal? 'z n)
                                         #f
                                         (even? (cdr n))))))
                      (even? '(s . z)))
                   q))
  '((#f)))

(test 'even?/odd?-4
  (run* (q) (evalo '(letrec ((even? (lambda (n)
                                      (if (equal? 'z n)
                                          #t
                                          (odd? (cdr n)))))
                             (odd? (lambda (n)
                                     (if (equal? 'z n)
                                         #f
                                         (even? (cdr n))))))
                      (even? '(s . (s . z))))
                   q))
  '((#t)))

(test 'even?/odd?-5
  (run* (q) (evalo '(letrec ((even? (lambda (n)
                                       (if (equal? 'z n)
                                           #t
                                           (odd? (cdr n)))))
                              (odd? (lambda (n)
                                      (if (equal? 'z n)
                                          #f
                                          (even? (cdr n))))))
                       (odd? '(s . (s . z))))
                    q))
  '((#f)))

(test 'proof-letrec-1
  (run 1 (prf)
    (fresh (rule assms ants)
      ;; We want to prove that C holds...
      (== `(,rule ,assms ,ants C) prf)
      ;; ...given the assumptions A, A => B, and B => C.
      (== `(A (if A B) (if B C)) assms)
      (evalo
       `(letrec ((member? (lambda (x ls)
                            (if (null? ls)
                                #f
                                (if (equal? (car ls) x)
                                    #t
                                    (member? x (cdr ls)))))))
          (letrec ((proof? (lambda (proof)
                             (match proof
                               [`(assumption ,assms () ,A)
                                (member? A assms)]
                               [`(modus-ponens
                                  ,assms
                                  ((,r1 ,assms ,ants1 (if ,A ,B))
                                   (,r2 ,assms ,ants2 ,A))
                                  ,B)
                                (and (proof? (list r1 assms ants1 (list 'if A B)))
                                     (proof? (list r2 assms ants2 A)))]))))
            (proof? ',prf)))
       #t)))
  '(((modus-ponens (A (if A B) (if B C))
                  ((assumption (A (if A B) (if B C)) () (if B C))
                   (modus-ponens (A (if A B) (if B C))
                                 ((assumption (A (if A B) (if B C)) () (if A B))
                                  (assumption (A (if A B) (if B C)) () A))
                                 B))
                  C))))


(test 'proof-begin-1
  (run 1 (q)
    (evalo '(begin
              (define member?
                (lambda (x ls)
                  (if (null? ls)
                      #f
                      (if (equal? (car ls) x)
                          #t
                          (member? x (cdr ls))))))
              (define proof?
                (lambda (proof)
                  (match proof
                    [`(assumption ,assms () ,A)
                     (member? A assms)]
                    [`(modus-ponens
                       ,assms
                       ((,r1 ,assms ,ants1 (if ,A ,B))
                        (,r2 ,assms ,ants2 ,A))
                       ,B)
                     (and (proof? (list r1 assms ants1 (list 'if A B)))
                          (proof? (list r2 assms ants2 A)))])))
              (proof? '(modus-ponens
                        (A (if A B) (if B C))
                        ((assumption (A (if A B) (if B C)) () (if B C))
                         (modus-ponens
                          (A (if A B) (if B C))
                          ((assumption (A (if A B) (if B C)) () (if A B))
                           (assumption (A (if A B) (if B C)) () A)) B))
                        C)))
           q))
  '((#t)))

(test 'proof-begin-2
  (run 1 (prf)
    (fresh (rule assms ants)
      ;; We want to prove that C holds...
      (== `(,rule ,assms ,ants C) prf)
      ;; ...given the assumptions A, A => B, and B => C.
      (== `(A (if A B) (if B C)) assms)
      (evalo `(begin
                (define member?
                  (lambda (x ls)
                    (if (null? ls)
                        #f
                        (if (equal? (car ls) x)
                            #t
                            (member? x (cdr ls))))))
                (define proof?
                  (lambda (proof)
                    (match proof
                      [`(assumption ,assms () ,A)
                       (member? A assms)]
                      [`(modus-ponens
                         ,assms
                         ((,r1 ,assms ,ants1 (if ,A ,B))
                          (,r2 ,assms ,ants2 ,A))
                         ,B)
                       (and (proof? (list r1 assms ants1 (list 'if A B)))
                            (proof? (list r2 assms ants2 A)))])))
                (proof? ',prf))
             #t)))
  '(((modus-ponens (A (if A B) (if B C))
                  ((assumption (A (if A B) (if B C)) () (if B C))
                   (modus-ponens (A (if A B) (if B C))
                                 ((assumption (A (if A B) (if B C)) () (if A B))
                                  (assumption (A (if A B) (if B C)) () A))
                                 B))
                  C))))

(test 'begin-append-1
  (run* (q)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                      s
                      (cons (car l)
                            (append (cdr l) s)))))
              (append '(1 2 3) '(4 5)))
           q))
  '(((1 2 3 4 5))))

(time-test 'begin-append-missing-first-recursive-arg-1
  (run 1 (q)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                      s
                      (cons (car l)
                            (append ,q s)))))
              (append '(1 2 3) '(4 5)))
           '(1 2 3 4 5)))
  '(((cdr l))))

(time-test 'begin-append-missing-first-recursive-arg-gensym-1
  (run 1 (q)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (== `(define append
               (lambda (l s)
                 (if (null? l)
                     s
                     (cons (car l)
                           (append ,q s)))))
            defn)
        (evalo `(begin
                  ,defn
                  (append '(,g1 ,g2 ,g3) '(,g4 ,g5)))
               `(,g1 ,g2 ,g3 ,g4 ,g5)))))
  '(((cdr l))))

(time-test 'begin-append-missing-second-recursive-arg-1
  (run 1 (q)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                      s
                      (cons (car l)
                            (append (cdr l) ,q)))))
              (append '(1 2 3) '(4 5)))
           '(1 2 3 4 5)))
  '((s)))

(time-test 'begin-append-missing-second-recursive-arg-gensym-1
  (run 1 (q)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5")))
      (fresh (defn)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (== `(define append
               (lambda (l s)
                 (if (null? l)
                     s
                     (cons (car l)
                           (append (cdr l) ,q)))))
            defn)
        (evalo `(begin
                  ,defn
                  (append '(,g1 ,g2 ,g3) '(,g4 ,g5)))
               `(,g1 ,g2 ,g3 ,g4 ,g5)))))
  '((s)))

(time-test "append-gensym-synthesis-with-cons-1"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (a b c d e f g)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (symbolo a)
        (symbolo c)
        (== `(define append
               (lambda (l s)
                 (if (null? l)
                     s
                     (cons (car l)
                           (append (,a ,b) ,c)))))
            defn)
        (evalo `(begin
                  ,defn
                  (cons (append '() '())
                        (cons (append '(,g6) '(,g7))
                              (cons (append '(,g1 ,g2 ,g3) '(,g4 ,g5))
                                    '()))))
               `(()
                 (,g6 ,g7)
                 (,g1 ,g2 ,g3 ,g4 ,g5))))))
  '(((define append
       (lambda (l s)
         (if (null? l) s (cons (car l) (append (cdr l) s))))))))

(time-test "append-gensym-synthesis-with-list-1"
  (run 1 (defn)
    (let ((g1 (gensym "g1"))
          (g2 (gensym "g2"))
          (g3 (gensym "g3"))
          (g4 (gensym "g4"))
          (g5 (gensym "g5"))
          (g6 (gensym "g6"))
          (g7 (gensym "g7")))
      (fresh (a b c d e f g)
        (absento g1 defn)
        (absento g2 defn)
        (absento g3 defn)
        (absento g4 defn)
        (absento g5 defn)
        (absento g6 defn)
        (absento g7 defn)
        (symbolo a)
        (symbolo c)
        (== `(define append
               (lambda (l s)
                 (if (null? l)
                     s
                     (cons (car l)
                           (append (,a ,b) ,c)))))
            defn)
        (evalo `(begin
                  ,defn
                  (list (append '() '())
                        (append '(,g6) '(,g7))
                        (append '(,g1 ,g2 ,g3) '(,g4 ,g5))))
               `(()
                 (,g6 ,g7)
                 (,g1 ,g2 ,g3 ,g4 ,g5))))))
  '(((define append
       (lambda (l s)
         (if (null? l) s (cons (car l) (append (cdr l) s))))))))

#|
;;; old timing:  doesn't come back even after 25 minutes
(time-test 'begin-append-missing-both-recursive-args-gensym-1 ;
(run 1 (q r)                            ;
(let ((g1 (gensym "g1"))                ;
(g2 (gensym "g2"))                      ;
(g3 (gensym "g3"))                      ;
(g4 (gensym "g4"))                      ;
(g5 (gensym "g5")))                     ;
(fresh (defn)                           ;
(absento g1 defn)                       ;
(absento g2 defn)                       ;
(absento g3 defn)                       ;
(absento g4 defn)                       ;
(absento g5 defn)                       ;
(== `(define append                     ;
(lambda (l s)                           ;
(if (null? l)                           ;
s                                       ;
(cons (car l)                           ;
(append ,q ,r)))))                      ;
defn)                                   ;
(evalo `(begin                          ;
,defn                                   ;
(append '(,g1 ,g2 ,g3) '(,g4 ,g5)))     ;
`(,g1 ,g2 ,g3 ,g4 ,g5)))))              ;
'((((cdr l) s))))                       ;
|#

(test "check quine"
  (run 1 (q)
       (== q '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))
       (evalo
         `(letrec ((eval-quasi (lambda (q eval)
                                 (match q
                                   [(? symbol? x) x]
                                   [`() '()]
                                   [`(,`unquote ,exp) (eval exp)]
                                   [`(quasiquote ,datum) ('error)]
                                   [`(,a . ,d)
                                     (cons (eval-quasi a eval) (eval-quasi d eval))]))))
            (letrec ((eval-expr
                       (lambda (expr env)
                         (match expr
                           [`(quote ,datum) datum]
                           [`(lambda (,(? symbol? x)) ,body)
                             (lambda (a)
                               (eval-expr body (lambda (y)
                                                 (if (equal? x y)
                                                   a
                                                   (env y)))))]
                           [(? symbol? x) (env x)]
                           [`(quasiquote ,datum)
                             (eval-quasi datum (lambda (exp) (eval-expr exp env)))]
                           [`(,rator ,rand)
                             ((eval-expr rator env) (eval-expr rand env))]
                           ))))
              (eval-expr ',q
                         'initial-env)))
         q))
  (list (list '((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))))


;;; old timing: this test used to take ~5 minutes
(printf "*** 'generate quine using Scheme-in-Scheme' test takes ~~12 minutes to run under Chez! ***\n")
(time
  (test "generate quine using Scheme-in-Scheme"
    (run 1 (q)
         (evalo
           `(letrec ((eval-quasi (lambda (q eval)
                                   (match q
                                     [(? symbol? x) x]
                                     [`() '()]
                                     [`(,`unquote ,exp) (eval exp)]
                                     [`(quasiquote ,datum) ('error)]
                                     [`(,a . ,d)
                                       (cons (eval-quasi a eval) (eval-quasi d eval))]))))
              (letrec ((eval-expr
                         (lambda (expr env)
                           (match expr
                             [`(quote ,datum) datum]
                             [`(lambda (,(? symbol? x)) ,body)
                               (lambda (a)
                                 (eval-expr body (lambda (y)
                                                   (if (equal? x y)
                                                     a
                                                     (env y)))))]
                             [(? symbol? x) (env x)]
                             [`(quasiquote ,datum)
                               (eval-quasi datum (lambda (exp) (eval-expr exp env)))]
                             [`(,rator ,rand)
                               ((eval-expr rator env) (eval-expr rand env))]
                             ))))
                (eval-expr ',q
                           'initial-env)))
           q))
    '((((lambda (_.0) `(,_.0 ',_.0)) '(lambda (_.0) `(,_.0 ',_.0))) (=/= ((_.0 closure)) ((_.0 prim))) (sym _.0)))))


#|
;;; This test used to take 2 minutes.  With pull request #2 (Prioritize symbol lookup)
;;; it no longer comes back after 10 minutes. The other tests are faster, though.
(printf "*** 'generate non-trivial quine old-fashioned way' test takes ~~2 minutes to run under Chez! ***\n")
(time
  (test "generate non-trivial quine old-fashioned way"
    (run 4 (q) (evalo q q))
    '((_.0 (num _.0))
      (#t)
      (#f)
      (((lambda (_.0) (list _.0 (list 'quote _.0)))
        '(lambda (_.0) (list _.0 (list 'quote _.0))))
       (=/= ((_.0 closure)) ((_.0 list)) ((_.0 prim)) ((_.0 quote))) (sym _.0)))))
|#


(time-test
 "fib-forward-1"
 (Barliman
  () ()
  (lambda (n)
    (if (zero? n) n
    (if (zero? (sub1 n)) n
    (+ (! (sub1 n)) (! (sub1 (sub1 n)))))))
  '(0 1 2 3 4 5 6)
  '(0 1 1 2 3 5 8))
  '((((define ! (lambda (n) (if (zero? n) n (if (zero? (sub1 n)) n (+ (! (sub1 n)) (! (sub1 (sub1 n))))))))))))

(time-test
 "fib-synthesis-1"
 (Barliman
  () (A)
  (lambda (n)
    (if (zero? n) ,A
    (if (zero? (sub1 n)) n
    (+ (! (sub1 n)) (! (sub1 (sub1 n)))))))
  '(0 1 2 3 4 5 6)
  '(0 1 1 2 3 5 8))
  '((((define ! (lambda (n) (if (zero? n) n (if (zero? (sub1 n)) n (+ (! (sub1 n)) (! (sub1 (sub1 n))))))))))))

(time-test
 "fib-synthesis-2"
 (Barliman
  () (A B)
  (lambda (n)
    (if (zero? n) ,A
    (if (zero? (sub1 n)) ,B
    (+ (! (sub1 n)) (! (sub1 (sub1 n)))))))
  '(0 1 2 3 4 5 6)
  '(0 1 1 2 3 5 8))
  '((((define ! (lambda (n) (if (zero? n) n (if (zero? (sub1 n)) n (+ (! (sub1 n)) (! (sub1 (sub1 n))))))))))))


(time-test
 "factorial-synthesis-nova-4h"
 (Barliman
  () (A B)
  (lambda (n) 
    (if (zero? n)
        1
        ,A))
  '(0 1 3 4)
  '(1 1 6 24))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

(printf "this test takes almost 10 minutes!\n")
(time-test
 "factorial-synthesis-nova-4i"
 (Barliman
  () (A)
  (lambda (n) 
    ,A)
  '(0 1 3 4)
  '(1 1 6 24))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

#| ;; Runs out of memory on Will's laptop
(time-test
 "factorial-synthesis-nova-4j"
 (Barliman
  () (A)
  ,A
  '(0 1 3 4)
  '(1 1 6 24))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))
|#

#!eof

(time-test
 "fib-synthesis-3"
 (Barliman
  () (A)
  (lambda (n)
    (if (zero? n) n
        (if (zero? (sub1 n)) n
            (+ (! ,A) (! (sub1 (sub1 n)))))))
  '(0 1 2 3 4 5 6)
  '(0 1 1 2 3 5 8))
  '((((define ! (lambda (n) (if (zero? n) n (if (zero? (sub1 n)) n (+ (! (sub1 n)) (! (sub1 (sub1 n))))))))))))

#!eof

(time-test
 "fib-synthesis-3"
 (Barliman
  () (A B C)
  (lambda (n)
    (if (zero? n) ,A
        (if (zero? (sub1 n)) ,B
            (+ (! (sub1 n)) (! ,C)))))
  '(0 1 2 3 4 5 6)
  '(0 1 1 2 3 5 8))
  '((((define ! (lambda (n) (if (zero? n) n (if (zero? (sub1 n)) n (+ (! (sub1 n)) (! (sub1 (sub1 n))))))))))))

#!eof

(time-test
 "factorial-synthesis-5b"
 (Barliman
   () (A B)
   (lambda (n) 
     (if ,A
         1
         (* n (! ,B))))
   '(0 5)
   '(1 120))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

(time-test
 "factorial-synthesis-4"
 (Barliman
   () (A)
   (lambda (n) 
     (if (zero? n)
         1
         (* n (! ,A))))
  '(0 1 2 3 5)
  '(1 1 2 6 120))
 '((((define ! (lambda (n) (if (zero? n) 1 (* n (! (sub1 n))))))))))

#!eof
