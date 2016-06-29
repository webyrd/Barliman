(load "chez-load-interp.scm")
(load "mk/test-check.scm")

(test 'append-empty
  (run 1 (q)
       (evalo
         `(begin
            (define append
              (lambda (l s)
                (if (null? l)
                  s
                  (cons (car l)
                        (append (cdr l) s)))))
            (append '() '()))
         '()))
  '((_.0)))

(test 'append-all-answers
  (run* (l1 l2)
        (evalo `(begin
                  (define append
                    (lambda (l s)
                      (if (null? l)
                        s
                        (cons (car l)
                              (append (cdr l) s)))))
                  (append ',l1 ',l2))
               '(1 2 3 4 5)))
  '(((() (1 2 3 4 5)))
    (((1) (2 3 4 5)))
    (((1 2) (3 4 5)))
    (((1 2 3) (4 5)))
    (((1 2 3 4) (5)))
    (((1 2 3 4 5) ()))))

;;; flipping rand/body eval order makes this one too hard,
;;; but dynamic ordering via eval-application fixes it!
(test 'append-cons-first-arg
  (run 1 (q)
;    (== q '(car l))
    (evalo `(begin (define append
                     (lambda (l s)
                       (if (null? l)
                         s
                         (cons ,q
                               (append (cdr l) s)))))
                   (append '(1 2 3) '(4 5)))
           '(1 2 3 4 5)))
  '(((car l))))

(test 'append-cdr-arg
  (run 1 (q)
       (evalo `(begin
                 (define append
                   (lambda (l s)
                     (if (null? l)
                       s
                       (cons (car l)
                             (append (cdr ,q) s)))))
                 (append '(1 2 3) '(4 5)))
              '(1 2 3 4 5)))
  '((l)))

(test 'append-cdr
  (run 1 (q)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                    s
                    (cons (car l)
                          (append (,q l) s)))))
              (append '(1 2 3) '(4 5)))
           '(1 2 3 4 5)))
  '((cdr)))

(time (test 'append-hard-1
  (run 1 (q r)
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                    s
                    (cons (car l)
                          (append (,q ,r) s)))))
              (append '(1 2 3) '(4 5)))
           '(1 2 3 4 5)))
  '(((cdr l)))))

(time (test 'append-hard-2
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
  '(((cdr l)))))

(time (test 'append-hard-3
  (run 1 (q r)
    (== q '(cdr l)) (== r 's)  ;; TODO
    (evalo `(begin
              (define append
                (lambda (l s)
                  (if (null? l)
                    s
                    (cons (car l)
                          (append ,q ,r)))))
              (list
                (append '(foo) '(bar))
                (append '(1 2 3) '(4 5))))
           (list '(foo bar) '(1 2 3 4 5))))
  '((((cdr l) s)))))

;(time (test 'append-hard-4
  ;(run 1 (q)
    ;(== q '((cdr l) s))  ;; TODO
    ;(evalo `(begin
              ;(define append
                ;(lambda (l s)
                  ;(if (null? l)
                    ;s
                    ;(cons (car l)
                          ;(append . ,q)))))
              ;(list
                ;(append '(foo) '(bar))
                ;(append '(1 2 3) '(4 5))))
           ;(list '(foo bar) '(1 2 3 4 5))))
  ;'((((cdr l) s)))))

;(time (test 'append-hard-5
  ;(run 1 (q r)
    ;(evalo `(begin
              ;(define append
                ;(lambda (l s)
                  ;(if (null? l)
                    ;s
                    ;(cons ,q
                          ;(append . ,r)))))
              ;(list
                ;(append '() '())
                ;(append '(foo) '(bar))
                ;(append '(1 2 3) '(4 5))))
           ;(list '() '(foo bar) '(1 2 3 4 5))))
  ;'(((car l) ((cdr l) s)))))

;(time (test 'append-hard-6
  ;(run 1 (q)
    ;(evalo `(begin
              ;(define append
                ;(lambda (l s)
                  ;(if (null? l)
                    ;s
                    ;(cons (car l) ,q))))
              ;(list
                ;(append '() '())
                ;(append '(foo) '(bar))
                ;(append '(1 2 3) '(4 5))))
           ;(list '() '(foo bar) '(1 2 3 4 5))))
  ;'(((append (cdr l) s)))))

;(test 'append-hard-7
  ;(run 1 (q r)
    ;(evalo `(begin
              ;(define append
                ;(lambda (l s)
                  ;(if (null? l)
                    ;s
                    ;(cons ,q ,r))))
              ;(list
                ;(append '() '())
                ;(append '(foo) '(bar))
                ;(append '(1 2 3) '(4 5))))
           ;(list '() '(foo bar) '(1 2 3 4 5))))
  ;'(((car l) (append (cdr l) s))))

;(test 'append-hard-8
  ;(run 1 (q)
    ;(evalo `(begin
              ;(define append
                ;(lambda (l s)
                  ;(if (null? l) s ,q)))
              ;(list
                ;(append '() '())
                ;(append '(foo) '(bar))
                ;(append '(1 2 3) '(4 5))))
           ;(list '() '(foo bar) '(1 2 3 4 5))))
  ;'(((cons (car l)  (append (cdr l) s)))))

;(time (test 'append-hard-8-gensym
  ;(run 1 (binding)
       ;(let ((g1 (gensym "g1"))
             ;(g2 (gensym "g2"))
             ;(g3 (gensym "g3"))
             ;(g4 (gensym "g4"))
             ;(g5 (gensym "g5"))
             ;(g6 (gensym "g6"))
             ;(g7 (gensym "g7")))
         ;(fresh (q)
           ;(absento g1 binding)
           ;(absento g2 binding)
           ;(absento g3 binding)
           ;(absento g4 binding)
           ;(absento g5 binding)
           ;(absento g6 binding)
           ;(absento g7 binding)
           ;(== `(lambda (l s) (if (null? l) s ,q))
               ;binding)
           ;(evalo
             ;`(begin (define append ,binding)
                ;(list
                  ;(append '() '())
                  ;(append '(,g1) '(,g2))
                  ;(append '(,g3 ,g4 ,g5) '(,g6 ,g7))))
             ;(list '() `(,g1 ,g2) `(,g3 ,g4 ,g5 ,g6 ,g7))))))
  ;'((lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s)))))))

;(time (test 'append-hard-9
  ;(run 1 (q r)
    ;(evalo `(begin
              ;(define append
                ;(lambda (l s)
                  ;(if (null? l) ,q ,r)))
              ;(list
                ;(append '() '())
                ;(append '(foo) '(bar))
                ;(append '(1 2 3) '(4 5))))
           ;(list '() '(foo bar) '(1 2 3 4 5))))
  ;'(((s (cons (car l) (append (cdr l) s)))))))

;; need better test examples
;; this starts producing nonsense results that game the test examples
;; example of its "cleverness":
;; (s (match s ((quasiquote ()) s)
;;             ((quasiquote (bar)) (quote (foo bar)))
;;             (_.0 (quote (1 2 3 4 5))) . _.1) _.2)
;(test 'append-hard-10
  ;(run 1 (q r s)
    ;(evalo `(begin
              ;(define
                ;(append (lambda (l s)
                          ;(if ,q ,r ,s))))
              ;(list
                ;(append '() '())
                ;(append '(foo) '(bar))
                ;(append '(1 2 3) '(4 5))))
           ;(list '() '(foo bar) '(1 2 3 4 5))))
  ;'())

;(test 'list-nth-element-peano
  ;(run 1 (q r)
    ;(evalo `(begin
              ;(define nth
                ;(lambda (n xs)
                  ;(if (null? n) ,q ,r)))
              ;(list
                ;(nth '() '(foo bar))
                ;(nth '(s) '(foo bar))
                ;(nth '() '(1 2 3))
                ;(nth '(s) '(1 2 3))
                ;(nth '(s s) '(1 2 3))))
           ;(list 'foo 'bar 1 2 3)))
  ;'(((car xs) (nth (cdr n) (cdr xs)))))

;(test 'reverse-hard-1
  ;(run 1 (q r s)
    ;(evalo `(begin
              ;(define append
                ;(lambda (l s)
                  ;(if (null? l) s
                    ;(cons (car l)
                          ;(append (cdr l) s)))))
              ;(begin
                ;(define reverse
                  ;(lambda (xs)
                    ;(if (null? xs) '()
                      ;(,q (reverse ,r) ,s))))
                ;(list
                  ;(reverse '())
                  ;(reverse '(a))
                  ;(reverse '(foo bar))
                  ;(reverse '(1 2 3)))))
           ;(list '() '(a) '(bar foo) '(3 2 1))))
  ;'((append (cdr xs) (list (car xs)))))

;(time (test 'reverse-hard-2
  ;(run 1 (q r s)
    ;(evalo `(begin
              ;(define append
                ;(lambda (l s)
                  ;(if (null? l) s
                    ;(cons (car l)
                          ;(append (cdr l) s)))))
              ;(begin
                ;(define reverse
                  ;(lambda (xs)
                    ;(if (null? xs) '()
                      ;(append (,q ,r) ,s))))
                ;(list
                  ;(reverse '())
                  ;(reverse '(a))
                  ;(reverse '(foo bar))
                  ;(reverse '(1 2 3)))))
           ;(list '() '(a) '(bar foo) '(3 2 1))))
  ;'((append (cdr xs) (list (car xs))))))
