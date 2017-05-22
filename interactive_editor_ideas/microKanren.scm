;; Barliman-compatible microKanren

;; convert from SICP style to Little Schemer style definition syntax
;; define assp
;; eqv? => equal?
;; quasiquote => explicit cons
;; add procedure? to Barliman
;; change representation of 'var' to avoid uses of vectors
;; make mzero, and several of the test definitons, functions of no args, since Barliman's 'define' expects a lambda
;; replace (+ c 1) with peano successor (and represent numbers as: z for 0, (s . z) for 1, etc.)
;; replace = with equal? in var=?

(define assp
  (lambda (p l)
    (cond
      ((null? l) #f)
      ((p (car (car l))) (car l))
      (else (assp p (cdr l))))))


(define var
  (lambda (c) (cons 'var c)))
(define var?
  (lambda (x) (and (pair? x) (equal? (car x) 'var))))
(define var=?
  (lambda (x1 x2) (equal? (cdr x1) (cdr x2))))


(define walk
  (lambda (u s)
    (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
      (if pr (walk (cdr pr) s) u))))

(define ext-s
  (lambda (x v s)
    (cons (cons x v) s)))

(define ==
  (lambda (u v)
    (lambda (s/c)
      (let ((s (unify u v (car s/c))))
        (if s (unit (cons s (cdr s/c))) (mzero))))))

(define unit
  (lambda (s/c)
    (cons s/c (mzero))))
(define mzero
  (lambda ()
    '()))

(define unify
  (lambda (u v s)
    (let ((u (walk u s)) (v (walk v s)))
      (cond
        ((and (var? u) (var? v) (var=? u v)) s)
        ((var? u) (ext-s u v s))
        ((var? v) (ext-s v u s))
        ((and (pair? u) (pair? v))
         (let ((s (unify (car u) (car v) s)))
           (and s (unify (cdr u) (cdr v) s))))
        (else (and (equal? u v) s))))))

(define call/fresh
  (lambda (f)
    (lambda (s/c)
      (let ((c (cdr s/c)))
        ((f (var c)) (cons (car s/c) (cons 's c)))))))

(define disj
  (lambda (g1 g2)
    (lambda (s/c) (mplus (g1 s/c) (g2 s/c)))))
(define conj
  (lambda (g1 g2)
    (lambda (s/c) (bind (g1 s/c) g2))))

(define mplus
  (lambda ($1 $2)
    (cond
      ((null? $1) $2)
      ((procedure? $1) (lambda () (mplus $2 ($1))))
      (else (cons (car $1) (mplus (cdr $1) $2))))))

(define bind
  (lambda ($ g)
    (cond
      ((null? $) (mzero))
      ((procedure? $) (lambda () (bind ($) g)))
      (else (mplus (g (car $)) (bind (cdr $) g))))))



;;; tests

(define empty-state
  (lambda ()
    '(() . z)))

(define pull
  (lambda ($)
    (if (procedure? $) (pull ($)) $)))

(define take-all
  (lambda ($)
    (let (($ (pull $)))
      (if (null? $) '() (cons (car $) (take-all (cdr $)))))))

(define take
  (lambda (n $)
    (if (equal? n 'z) '()
        (let (($ (pull $)))
          (if (null? $) '() (cons (car $) (take (cdr n) (cdr $))))))))

(define a-and-b
  (lambda ()
    (conj 
     (call/fresh (lambda (a) (== a 7)))
     (call/fresh 
      (lambda (b) 
        (disj
         (== b 5)
         (== b 6)))))))

(define fives
  (lambda (x)
    (disj
     (== x 5)      
     (lambda (a/c)
       (lambda ()
         ((fives x) a/c))))))

(define appendo
  (lambda (l s out)
    (disj
     (conj (== '() l) (== s out))
     (call/fresh
      (lambda (a)
        (call/fresh
         (lambda (d)
           (conj
            (== (cons a d) l)
            (call/fresh
             (lambda (res)
               (conj
                (== (cons a res) out)
                (lambda (s/c)
                  (lambda ()
                    ((appendo d s res) s/c))))))))))))))

(define appendo2
  (lambda (l s out)
    (disj
     (conj (== '() l) (== s out))
     (call/fresh
      (lambda (a)
        (call/fresh
         (lambda (d)
           (conj
            (== (cons a d) l)
            (call/fresh
             (lambda (res)
               (conj
                (lambda (s/c)
                  (lambda ()
                    ((appendo2 d s res) s/c)))
                (== (cons a res) out))))))))))))

(define call-appendo
  (lambda ()
    (call/fresh
     (lambda (q)
       (call/fresh
        (lambda (l)
          (call/fresh
           (lambda (s)
             (call/fresh
              (lambda (out)
                (conj
                 (appendo l s out)
                 (== (list l s out) q))))))))))))

(define call-appendo2
  (lambda ()
    (call/fresh
     (lambda (q)
       (call/fresh
        (lambda (l)
          (call/fresh
           (lambda (s)
             (call/fresh
              (lambda (out)
                (conj
                 (appendo2 l s out)
                 (== (list l s out) q))))))))))))

(define call-appendo3
  (lambda ()
    (call/fresh
     (lambda (q)
       (call/fresh
        (lambda (l)
          (call/fresh
           (lambda (s)
             (call/fresh
              (lambda (out)
                (conj
                 (== (list l s out) q)
                 (appendo l s out))))))))))))

(define ground-appendo
  (lambda ()
    (appendo '(a) '(b) '(a b))))

(define ground-appendo2
  (lambda ()
    (appendo2 '(a) '(b) '(a b))))

(define relo
  (lambda (x)
    (call/fresh
     (lambda (x1)
       (call/fresh
        (lambda (x2)
          (conj
           (== x (cons x1 x2))
           (disj
            (== x1 x2)
            (lambda (s/c)
              (lambda () ((relo x) s/c)))))))))))

(define many-non-ans
  (lambda ()
    (call/fresh
     (lambda (x)
       (disj
        (relo (cons 5 6))
        (== x 3))))))


#!eof

"second-set t1"
(let (($ ((call/fresh (lambda (q) (== q 5))) (empty-state)))) (car $))
=>
'((((var . z) . 5)) . (s . z))

"second-set t2"
(let (($ ((call/fresh (lambda (q) (== q 5))) (empty-state))))
  (cdr $))
'()

"second-set t3"
(let (($ ((a-and-b) (empty-state))))
  (car $))
'((((var . (s . z)) . 5) ((var . z) . 7)) . (s . (s . z)))

"second-set t3, take"
(let (($ ((a-and-b) (empty-state))))
  (take '(s . z) $))
'((((var . (s . z)) . 5) ((var . z) . 7)) . (s . (s . z)))

"who cares"
(let (($ ((call/fresh (lambda (q) (fives q))) (empty-state))))
  (car $))
'((((var . z) . 5)) . (s . z))

"take-all a-and-b stream"
(let (($ ((a-and-b) (empty-state))))
  (take-all $))
'(((((var . (s . z)) . 5) ((var . z) . 7)) . (s . (s . z)))
  ((((var . (s . z)) . 6) ((var . z) . 7)) . (s . (s . z))))

"second-set t4"
(let (($ ((a-and-b) (empty-state))))
  (car (cdr $)))
'((((var . (s . z)) . 6) ((var . z) . 7)) . (s . (s . z)))

"second-set t5"
(let (($ ((a-and-b) (empty-state))))
  (cdr (cdr $)))
'()

"who cares"
(let (($ ((call/fresh (lambda (q) (fives q))) (empty-state))))
  (take '(s . z) $))
'(((((var . z) . 5)) . (s . z)))

"take 2 a-and-b stream"
(let (($ ((a-and-b) (empty-state))))
  (take '(s . (s . z)) $))
'(((((var . (s . z)) . 5) ((var . z) . 7)) . (s . (s . z)))
  ((((var . (s . z)) . 6) ((var . z) . 7)) . (s . (s . z))))



"ground appendo"
(car (((ground-appendo) (empty-state))))
'((((var . (s . (s . z))) . (b)) ((var . (s . z)) . ()) ((var . z) . a)) . (s . (s . (s . z))))

"ground appendo2"
(car (((ground-appendo2) (empty-state))))
'((((var . (s . (s . z))) . (b)) ((var . (s . z)) . ()) ((var . z) . a)) . (s . (s . (s . z))))

"appendo"
(take '(s . (s . z)) ((call-appendo) (empty-state)))
'(((((var . z) (var s . z) (var s s . z) (var s s s . z))
    ((var s s . z) var s s s . z)
    ((var s . z))) s s s s . z)
  ((((var . z) (var s . z) (var s s . z) (var s s s . z))
    ((var s s . z) var s s s s s s . z)
    ((var s s s s s . z))
    ((var s s s . z) (var s s s s . z) var s s s s s s . z)
    ((var s . z) (var s s s s . z) var s s s s s . z)) s s s s s
    s s . z))

'(((((var . z) (var s . z) (var s s . z) (var s s s . z)) ((var s s . z) var s s s . z) ((var s . z))) s s s s . z) ((((var . z) (var s . z) (var s s . z) (var s s s . z)) ((var s s . z) var s s s s s s . z) ((var s s s s s . z)) ((var s s s . z) (var s s s s . z) var s s s s s s . z) ((var s . z) (var s s s s . z) var s s s s s . z)) s s s s s s s . z))


"appendo2"
(take '(s . (s . z)) ((call-appendo2) (empty-state)))
'(((((var . z) (var s . z) (var s s . z) (var s s s . z))
    ((var s s . z) var s s s . z)
    ((var s . z))) s s s s . z)
  ((((var . z) (var s . z) (var s s . z) (var s s s . z))
    ((var s s s . z) (var s s s s . z) var s s s s s s . z)
    ((var s s . z) var s s s s s s . z)
    ((var s s s s s . z))
    ((var s . z) (var s s s s . z) var s s s s s . z)) s s s s s
    s s . z))



"many non-ans"
  (take '(s . z) ((many-non-ans) (empty-state)))
  '(((((var . z) . 3)) . (s . z)))





;;; hmmm--reify seems tricky due to reify-name and string/symbol manipulations
(test-check "reify-1st across appendo"
  (map reify-1st (take 2 (call-appendo empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

(test-check "reify-1st across appendo2"
  (map reify-1st (take 2 (call-appendo2 empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))


#!eof

;; From https://raw.githubusercontent.com/jasonhemann/microKanren/master/microKanren.scm

;; Jason Hemann and Dan Friedman
;; microKanren, final implementation from paper

(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s) `((,x . ,v) . ,s))

(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))

;;;;;;;;;;;;;;;;;;;;;;;;

;; test programs, from https://raw.githubusercontent.com/jasonhemann/microKanren/master/microKanren-test-programs.scm

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (errorf 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

(define a-and-b
  (conj 
   (call/fresh (lambda (a) (== a 7)))
   (call/fresh 
    (lambda (b) 
      (disj
       (== b 5)
       (== b 6))))))

(define fives
  (lambda (x)
    (disj
     (== x 5)      
     (lambda (a/c)
       (lambda ()
         ((fives x) a/c))))))

(define appendo
  (lambda (l s out)
    (disj
     (conj (== '() l) (== s out))
     (call/fresh
      (lambda (a)
        (call/fresh
         (lambda (d)
           (conj
            (== `(,a . ,d) l)
            (call/fresh
             (lambda (res)
               (conj
                (== `(,a . ,res) out)
                (lambda (s/c)
                  (lambda ()
                    ((appendo d s res) s/c))))))))))))))

(define appendo2
  (lambda (l s out)
    (disj
     (conj (== '() l) (== s out))
     (call/fresh
      (lambda (a)
        (call/fresh
         (lambda (d)
           (conj
            (== `(,a . ,d) l)
            (call/fresh
             (lambda (res)
               (conj
                (lambda (s/c)
                  (lambda ()
                    ((appendo2 d s res) s/c)))
                (== `(,a . ,res) out))))))))))))

(define call-appendo
  (call/fresh
   (lambda (q)
     (call/fresh
      (lambda (l)
        (call/fresh
         (lambda (s)
           (call/fresh
            (lambda (out)
              (conj
               (appendo l s out)
               (== `(,l ,s ,out) q)))))))))))

(define call-appendo2
  (call/fresh
   (lambda (q)
     (call/fresh
      (lambda (l)
        (call/fresh
         (lambda (s)
           (call/fresh
            (lambda (out)
              (conj
               (appendo2 l s out)
               (== `(,l ,s ,out) q)))))))))))

(define call-appendo3
  (call/fresh
   (lambda (q)
     (call/fresh
      (lambda (l)
        (call/fresh
         (lambda (s)
           (call/fresh
            (lambda (out)
              (conj
               (== `(,l ,s ,out) q)
               (appendo l s out)))))))))))

(define ground-appendo (appendo '(a) '(b) '(a b)))

(define ground-appendo2  (appendo2 '(a) '(b) '(a b)))

(define relo
  (lambda (x)
    (call/fresh
     (lambda (x1)
       (call/fresh
        (lambda (x2)
          (conj
           (== x `(,x1 . ,x2))
           (disj
            (== x1 x2)
            (lambda (s/c)
              (lambda () ((relo x) s/c)))))))))))

(define many-non-ans
  (call/fresh
   (lambda (x)
     (disj
      (relo `(5 . 6))
      (== x 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test cases from https://raw.githubusercontent.com/jasonhemann/microKanren/master/microKanren-test.scm

(load "microKanren-test-programs.scm")

(test-check "second-set t1"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (car $))
  '(((#(0) . 5)) . 1))

(test-check "second-set t2"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (cdr $))
  '())

(test-check "second-set t3"
  (let (($ (a-and-b empty-state)))
    (car $))
  '(((#(1) . 5) (#(0) . 7)) . 2))

(test-check "second-set t3, take"
  (let (($ (a-and-b empty-state)))
    (take 1 $))
  '((((#(1) . 5) (#(0) . 7)) . 2)))

(test-check "second-set t4"
  (let (($ (a-and-b empty-state)))
    (car (cdr $)))
  '(((#(1) . 6) (#(0) . 7)) . 2))

(test-check "second-set t5"
  (let (($ (a-and-b empty-state)))
    (cdr (cdr $)))
  '())

(test-check "who cares"
  (let (($ ((call/fresh (lambda (q) (fives q))) empty-state)))
    (take 1 $))
  '((((#(0) . 5)) . 1)))

(test-check "take 2 a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (take 2 $))
  '((((#(1) . 5) (#(0) . 7)) . 2)
    (((#(1) . 6) (#(0) . 7)) . 2)))

(test-check "take-all a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (take-all $))
  '((((#(1) . 5) (#(0) . 7)) . 2)
    (((#(1) . 6) (#(0) . 7)) . 2)))

(test-check "ground appendo"
  (car ((ground-appendo empty-state)))
  '(((#(2) b) (#(1)) (#(0) . a)) . 3))

(test-check "ground appendo2"
  (car ((ground-appendo2 empty-state)))
  '(((#(2) b) (#(1)) (#(0) . a)) . 3))

(test-check "appendo"
  (take 2 (call-appendo empty-state))
  '((((#(0) #(1) #(2) #(3)) (#(2) . #(3)) (#(1))) . 4)
    (((#(0) #(1) #(2) #(3)) (#(2) . #(6)) (#(5)) (#(3) #(4) . #(6)) (#(1) #(4) . #(5))) . 7)))

(test-check "appendo2"
  (take 2 (call-appendo2 empty-state))
  '((((#(0) #(1) #(2) #(3)) (#(2) . #(3)) (#(1))) . 4) (((#(0) #(1) #(2) #(3)) (#(3) #(4) . #(6)) (#(2) . #(6)) (#(5)) (#(1) #(4) . #(5))) . 7)))

(test-check "reify-1st across appendo"
  (map reify-1st (take 2 (call-appendo empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

(test-check "reify-1st across appendo2"
  (map reify-1st (take 2 (call-appendo2 empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

(test-check "many non-ans"
  (take 1 (many-non-ans empty-state))
  '((((#(0) . 3)) . 1)))
