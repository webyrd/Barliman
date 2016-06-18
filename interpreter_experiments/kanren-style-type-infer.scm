;;; WEB -- 18 June 2016

;; Type inferencer in miniKanren, adapted from Oleg's Kanren polymorphic type inferencer
;;
;; http://kanren.cvs.sourceforge.net/viewvc/kanren/kanren/examples/type-inference.scm?view=markup
;;
;; Unlike the Kanren inferencer, this definition of !- is a pure
;; relation, with no cuts and no uses of project.  This inferencer
;; also does not require a parser/unparser, and allows shadowing.



;;; TODO FIXME -- use initial environment to hold primitive functions, just like in evaluator and parser

(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        ((== y x))
        ((=/= y x) (membero x rest))))))

(define type-lookupo
  (lambda (g x t)
    (fresh (tq)
      (membero `(,x . ,tq) g)
      (conde
        ((== `(non-generic ,t) tq))
        ((fresh (g^ rand^)
           (== `(generic ,g^ ,rand^) tq)
           (!- g^ rand^ t)))))))

(define int 'int)
(define bool 'bool)

(define !-
  (lambda (g expr t)
    (conde
      ((symbolo expr) (type-lookupo g expr t))
      ((numbero expr) (== int t))
      ((== #f expr) (== bool t))
      ((== #t expr) (== bool t))
      ((fresh (e)
         (== `(zero? ,e) expr)
         (== bool t)
         (not-in-envo 'zero? g)
         (!- g e int)))
      ((fresh (e)
         (== `(sub1 ,e) expr)
         (== int t)
         (not-in-envo 'sub1 g)
         (!- g e int)))
      ((fresh (e1 e2)
         (== `(+ ,e1 ,e2) expr)
         (== int t)
         (not-in-envo '+ g)
         (!- g e1 int)
         (!- g e2 int)))
      ((fresh (e1 e2 e3)
         (== `(if ,e1 ,e2 ,e3) expr)
         (not-in-envo 'if g)
         (!- g e1 bool)
         (!- g e2 t)
         (!- g e3 t)))
      ((fresh (x body t-x t-body)
         (== `(lambda (,x) ,body) expr)
         (symbolo x)
         (== `(-> ,t-x ,t-body) t)         
         (not-in-envo 'lambda g)
         (!- `((,x non-generic ,t-x) . ,g) body t-body)))
      ((fresh (rator rand t-rand)
         (== `(,rator ,rand) expr)
         (!- g rator `(-> ,t-rand ,t))
         (!- g rand t-rand)))
      ((fresh (rand)
         (== `(fix ,rand) expr)
         (not-in-envo 'fix g)
         (!- g rand `(-> ,t ,t))))
      ((fresh (x rand body)
         ;; polylet
         (== `(let ((,x ,rand)) ,body) expr)
         (not-in-envo 'let g)
         (fresh (some-type)
           (!- g rand some-type))
         (!- `((,x generic ,g ,rand) . ,g) body t))))))



;;; reused definitions from interpreter:

(define empty-env '())

(define (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))


;;; tests

(test 'test-!-1
  (and
    (equal?
      (run* (q) (!- '() '17 int))
      '(_.0))
    (equal?
      (run* (q) (!- '() '17 q))
      '(int)))
  #t)

(test 'arithmetic-primitives
  (run* (q) (!- '() '(zero? 24) q))
  '(bool))

(test 'test-!-sub1
  (run* (q) (!- '() '(zero? (sub1 24)) q))
  '(bool))

(test 'test-!-+
  (run* (q)
    (!- '() '(zero? (sub1 (+ 18 (+ 24 50)))) q))
  '(bool))

(test 'test-!-2
  (and
    (equal?
      (run* (q) (!- '() '(zero? 24) q))
      '(bool))
    (equal?
      (run* (q) (!- '() '(zero? (+ 24 50)) q))
      '(bool))
    (equal?
      (run* (q)
	(!- '() '(zero? (sub1 (+ 18 (+ 24 50)))) q))
      '(bool)))
  #t)

(test 'test-!-3
  (run* (q) (!- '() '(if (zero? 24) 3 4) q))
  '(int))

(test 'if-expressions
  (run* (q)
    (!- '() '(if (zero? 24) (zero? 3) (zero? 4)) q))
  '(bool))

(test 'variables
  (and
    (equal?
      (run* (q)
        (type-lookupo '((b non-generic int) (a non-generic bool)) 'a q))
      '(bool))
    (equal?
      (run* (q)
	(!- '((a non-generic int)) '(zero? a) q))
      '(bool))
    (equal?
      (run* (q)
	(!- '((b non-generic bool) (a non-generic int))
            '(zero? a)
            q))
      '(bool)))
  #t)

(test 'variables-4a
  (run* (q)
    (!- '((b non-generic bool) (a non-generic int))
        '(lambda (x) (+ x 5))
        q))
  '((-> int int)))

(test 'variables-4b
  (run* (q)
    (!- '((b non-generic bool) (a non-generic int))
        '(lambda (x) (+ x a))
        q))
  '((-> int int)))

(test 'variables-4c
  (run* (q)
    (!- '() '(lambda (a) (lambda (x) (+ x a))) q))
  '((-> int (-> int int))))

(test 'everything-but-polymorphic-let
  (run* (q)
    (!- '() '(lambda (f)
               (lambda (x)
                 ((f x) x)))
        q))
  '((->
     (-> _.0 (-> _.0 _.1))
     (-> _.0 _.1))))

(test 'everything-but-polymorphic-let
  (run* (q)
    (!- '()
        '((fix (lambda (sum)
                 (lambda (n)
                   (if (zero? n)
                       0
                       (+ n (sum (sub1 n)))))))
          10)
        q))
  '(int))

(test 'everything-but-polymorphic-let
  (run* (q)
    (!- '()
        '((fix (lambda (sum)
                 (lambda (n)
                   (+ n (sum (sub1 n))))))
          10)
        q))
  '(int))

(test 'everything-but-polymorphic-let
  (run* (q)
    (!- '()
        '((lambda (f)
            (if (f (zero? 5))
                (+ (f 4) 8)
                (+ (f 3) 7)))
          (lambda (x) x))
        q))
  '())

(test 'polymorphic-let
  (run* (q)
    (!- '()
        '(let ((f (lambda (x) x)))
           (if (f (zero? 5))
               (+ (f 4) 8)
               (+ (f 3) 7)))
        q))
  '(int))

(test 'with-robust-syntax
  (run* (q)
    (!- '()
        '((fix
           (lambda (sum)
             (lambda (n)
               (if (if (zero? n) #t #f)
                   0
                   (+ n (sum (sub1 n)))))))
           10)
        q))
  '(int))

(test 'with-robust-syntax-but-long-jumps/poly-let
  (run* (q)
    (!- '()
        '(let ((f (lambda (x) x)))
           (if (f (zero? 5))
               (+ (f 4) 8)
               (+ (f 3) 7)))
        q))
  '(int))

(test 'type-habitation-1
  (run 1 (g e)
    (!- g e '(-> int int)))
  '(((((_.0 non-generic (-> int int)) . _.1) _.0) (sym _.0))))

(test 'type-habitation-2
  (run 1 (g h r q z y t)
    (!- g `(,h ,r (,q ,z ,y)) t))
  '(((()
      +
      _.0
      +
      _.1
      _.2
      int)
     (num _.0 _.1 _.2)))
)

(test 'type-habitation-2b
  (run 1 (g h r q z y t)
    (symbolo r)
    (!- g `(,h ,r (,q ,z ,y)) t))
  '(((((_.0 non-generic int))
      +
      _.0
      +
      _.1
      _.2
      int)
     (=/= ((_.0 +)))
     (num _.1 _.2)
     (sym _.0)))
)

(test 'type-habitation-3
  (and
    (equal?
      (run 1 (la f b)
	(!- '() `(,la (,f) ,b) '(-> int int)))
      '(((lambda _.0 _.1) (num _.1) (sym _.0))))
    (equal?
     (run 1 (expr type)
       (fresh (h r q z y t u v)
         (== `(,h ,r (,q ,z ,y)) expr)
         (== `(,t ,u ,v) type)
         (!- '() expr type)))
     '((((lambda (_.0) (+ _.1 _.2)) (-> _.3 int))
        (=/= ((_.0 +)))
        (num _.1 _.2)
        (sym _.0)))))
  #t)


;;; Will's tests

;;; adapted from:
;;;
;;; Polymorphic Types
;;; and Type Inference Programming Languages CS442
;;; David Toman
;;; School of Computer Science University of Waterloo
;;;
;;; https://cs.uwaterloo.ca/~david/cs442/lect-TYPEINFERENCE.pdf
(test 'w-let-poly-1
  (run* (q) 
    (!- '()
        '(let ((d (lambda (f) (lambda (x) (f (f x))))))
           (let ((a1 ((d (lambda (x) (sub1 x))) 2)))
             (let ((a2 ((d (lambda (x) (if x #t #f))) #t)))
               d)))
        q))
  '((-> (-> _.0 _.0) (-> _.0 _.0))))

(test 'w-let-poly-2
  (run* (q) 
    (!- '()
        '(let ((d (lambda (f) (lambda (x) (f (f x))))))
           (let ((a1 ((d (lambda (x) (sub1 x))) 2)))
             (let ((a2 (d (lambda (x) (if x #t #f)))))
               a1)))
        q))
  '(int))

(test 'w-let-poly-3
  (run* (q) 
    (!- '()
        '(let ((d (lambda (f) (lambda (x) (f (f x))))))
           (let ((a1 ((d (lambda (x) (sub1 x))) 2)))
             (let ((a2 (d (lambda (x) (if x #t #f)))))
               a2)))
        q))
  '((-> bool bool)))

(test 'w-let-poly-4
  (run* (q) 
    (!- '()
        '(let ((d (lambda (f) (lambda (x) (f (f x))))))
           (let ((a1 ((d (lambda (x) (sub1 x))) 2)))
             (let ((a2 ((d (lambda (x) (if x #t #f))) #t)))
               a2)))
        q))
  '(bool))


#|
;; need to add lists

(test 'w-length-1
  (run* (q) 
    (!- '()
        '((fix
           (lambda (length)
             (lambda (l)
               (if (if (null? l) #t #f)
                   0
                   (+ 1 (length (cdr l)))))))
          (cons 1 (cons 2 (cons 3))))
      q))
  '(int))
|#
