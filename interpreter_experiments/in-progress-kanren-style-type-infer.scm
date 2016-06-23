;;; WEB -- 18 June 2016

;; Type inferencer in miniKanren, adapted from Oleg's Kanren polymorphic type inferencer
;;
;; http://kanren.cvs.sourceforge.net/viewvc/kanren/kanren/examples/type-inference.scm?view=markup
;;
;; Unlike the Kanren inferencer, this definition of !- is a pure
;; relation, with no cuts and no uses of project.  This inferencer
;; also does not require a parser/unparser, and allows shadowing.



;;; TODO FIXME -- use initial environment to hold primitive functions,
;;; just like in evaluator and parser

;;; TODO -- add 'letrec', 'begin', multi-argument application,
;;; variadic and multi-argument lambda, lists, quote, match, and the
;;; primitives from the evaluator

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
           (!-auxo g^ rand^ t)))))))

(define int 'int)
(define bool 'bool)

(define !-
  (lambda (expr t)
    (!-auxo initial-type-env expr t)))

(define !-aux
  (lambda (g expr t)
    (conde
      ((symbolo expr) (type-lookupo g expr t))
      ((numbero expr) (== int t))

      ;;; from Oleg's inferencer --
      ;;; not needed for miniScheme
      ((fresh (e)
         (== `(zero? ,e) expr)
         (== bool t)
         (not-in-envo 'zero? g)
         (!-auxo g e int)))
      ((fresh (e)
         (== `(sub1 ,e) expr)
         (== int t)
         (not-in-envo 'sub1 g)
         (!-auxo g e int)))
      ((fresh (e1 e2)
         (== `(+ ,e1 ,e2) expr)
         (== int t)
         (not-in-envo '+ g)
         (!-auxo g e1 int)
         (!-auxo g e2 int)))

#|
      ;; TODO implement!
      ;;
      ;; I think I need to add lists to be able to handle variadic.
      ((fresh (x body)
         (== `(lambda ,x ,body) expr)
         (conde
           ;; Variadic
           ((symbolo x))
           ;; Multi-argument
           ((list-of-symbolso x)))
         (not-in-envo 'lambda env)))
|#
      
      ((fresh (x body t-x t-body)
         (== `(lambda (,x) ,body) expr)
         (symbolo x)
         (== `(-> ,t-x ,t-body) t)         
         (not-in-envo 'lambda g)
         (!-auxo `((,x non-generic ,t-x) . ,g) body t-body)))


      
      ;; TODO -- multi-arg application and prim app
      ((fresh (rator rand t-rand)
         (== `(,rator ,rand) expr)
         (!-auxo g rator `(-> ,t-rand ,t))
         (!-auxo g rand t-rand)))

      ;; TODO -- replace with 'letrec'
      ((fresh (rand)
         (== `(fix ,rand) expr)
         (not-in-envo 'fix g)
         (!-auxo g rand `(-> ,t ,t))))
      
      ((fresh (x rand body)
         ;; polylet
         (== `(let ((,x ,rand)) ,body) expr)
         (not-in-envo 'let g)
         (fresh (some-type)
           (!-auxo g rand some-type))
         (!-auxo `((,x generic ,g ,rand) . ,g) body t)))
      
      ((!-primo g expr t)))))


;; TODO -- update this to be prim-!o or whatever
(define (eval-primo prim-id a* val)
  (conde
    [(== prim-id 'cons)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== `(,a . ,d) val))]
    [(== prim-id 'car)
     (fresh (d)
       (== `((,val . ,d)) a*)
       (=/= 'closure val))]
    [(== prim-id 'cdr)
     (fresh (a)
       (== `((,a . ,val)) a*)
       (=/= 'closure a))]
    [(== prim-id 'not)
     (fresh (b)
       (== `(,b) a*)
       (conde
         ((=/= #f b) (== #f val))
         ((== #f b) (== #t val))))]
    [(== prim-id 'equal?)
     (fresh (v1 v2)
       (== `(,v1 ,v2) a*)
       (conde
         ((== v1 v2) (== #t val))
         ((=/= v1 v2) (== #f val))))]
    [(== prim-id 'symbol?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((symbolo v) (== #t val))
         ((numbero v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)))))]
    [(== prim-id 'null?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((== '() v) (== #t val))
         ((=/= '() v) (== #f val))))]))



(define (!-primo g expr t)
  (conde
    ((!-booleano g expr t))
    ((!-ando g expr t))
    ((!-oro g expr t))
    ((!-ifo g expr t))))

(define (!-booleano g expr t)
  (fresh ()
    (== bool t)
    (conde
      ((== #t expr))
      ((== #f expr)))))

(define (!-ando g expr t)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (== bool t)
    (not-in-envo 'and env)
    (!-and-auxo g e*)))

(define (!-and-auxo g e*)
  (conde
    ((== '() e*))
    ((fresh (e e-rest)
       (== `(,e . ,e-rest) e*)
       (!-auxo g e bool)
       (!-and-auxo g e-rest)))))

(define (!-oro g expr t)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (== bool t)
    (not-in-envo 'or env)
    (!-or-auxo g e*)))

(define (!-or-auxo g e*)
  (conde
    ((== '() e*))
    ((fresh (e e-rest)
       (== `(,e . ,e-rest) e*)
       (!-auxo g e bool)
       (!-or-auxo g e-rest)))))

(define (!-ifo g expr t)
  (fresh (e1 e2 e3)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (!-auxo g e1 bool)
    (!-auxo g e2 t)
    (!-auxo g e3 t)))

;; TODO -- update to include types
(define initial-type-env `((list . (val . (closure (lambda x x) ,empty-env)))
                           (not . (val . (prim . not)))
                           (equal? . (val . (prim . equal?)))
                           (symbol? . (val . (prim . symbol?)))
                           (cons . (val . (prim . cons)))
                           (null? . (val . (prim . null?)))
                           (car . (val . (prim . car)))
                           (cdr . (val . (prim . cdr)))
                           . ,empty-env))








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

(test 'test-!-aux1
  (and
    (equal?
      (run* (q) (!-auxo '() '17 int))
      '(_.0))
    (equal?
      (run* (q) (!-auxo '() '17 q))
      '(int)))
  #t)

(test 'arithmetic-primitives
  (run* (q) (!-auxo '() '(zero? 24) q))
  '(bool))

(test 'test-!-auxsub1
  (run* (q) (!-auxo '() '(zero? (sub1 24)) q))
  '(bool))

(test 'test-!-aux+
  (run* (q)
    (!-auxo '() '(zero? (sub1 (+ 18 (+ 24 50)))) q))
  '(bool))

(test 'test-!-aux2
  (and
    (equal?
      (run* (q) (!-auxo '() '(zero? 24) q))
      '(bool))
    (equal?
      (run* (q) (!-auxo '() '(zero? (+ 24 50)) q))
      '(bool))
    (equal?
      (run* (q)
	(!-auxo '() '(zero? (sub1 (+ 18 (+ 24 50)))) q))
      '(bool)))
  #t)

(test 'test-!-aux3
  (run* (q) (!-auxo '() '(if (zero? 24) 3 4) q))
  '(int))

(test 'if-expressions
  (run* (q)
    (!-auxo '() '(if (zero? 24) (zero? 3) (zero? 4)) q))
  '(bool))

(test 'variables
  (and
    (equal?
      (run* (q)
        (type-lookupo '((b non-generic int) (a non-generic bool)) 'a q))
      '(bool))
    (equal?
      (run* (q)
	(!-auxo '((a non-generic int)) '(zero? a) q))
      '(bool))
    (equal?
      (run* (q)
	(!-auxo '((b non-generic bool) (a non-generic int))
            '(zero? a)
            q))
      '(bool)))
  #t)

(test 'variables-4a
  (run* (q)
    (!-auxo '((b non-generic bool) (a non-generic int))
        '(lambda (x) (+ x 5))
        q))
  '((-> int int)))

(test 'variables-4b
  (run* (q)
    (!-auxo '((b non-generic bool) (a non-generic int))
        '(lambda (x) (+ x a))
        q))
  '((-> int int)))

(test 'variables-4c
  (run* (q)
    (!-auxo '() '(lambda (a) (lambda (x) (+ x a))) q))
  '((-> int (-> int int))))

(test 'everything-but-polymorphic-let
  (run* (q)
    (!-auxo '() '(lambda (f)
               (lambda (x)
                 ((f x) x)))
        q))
  '((->
     (-> _.0 (-> _.0 _.1))
     (-> _.0 _.1))))

(test 'everything-but-polymorphic-let
  (run* (q)
    (!-auxo '()
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
    (!-auxo '()
        '((fix (lambda (sum)
                 (lambda (n)
                   (+ n (sum (sub1 n))))))
          10)
        q))
  '(int))

(test 'everything-but-polymorphic-let
  (run* (q)
    (!-auxo '()
        '((lambda (f)
            (if (f (zero? 5))
                (+ (f 4) 8)
                (+ (f 3) 7)))
          (lambda (x) x))
        q))
  '())

(test 'polymorphic-let
  (run* (q)
    (!-auxo '()
        '(let ((f (lambda (x) x)))
           (if (f (zero? 5))
               (+ (f 4) 8)
               (+ (f 3) 7)))
        q))
  '(int))

(test 'with-robust-syntax
  (run* (q)
    (!-auxo '()
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
    (!-auxo '()
        '(let ((f (lambda (x) x)))
           (if (f (zero? 5))
               (+ (f 4) 8)
               (+ (f 3) 7)))
        q))
  '(int))

(test 'type-habitation-1
  (run 1 (g e)
    (!-auxo g e '(-> int int)))
  '(((((_.0 non-generic (-> int int)) . _.1) _.0) (sym _.0))))

(test 'type-habitation-2
  (run 1 (g h r q z y t)
    (!-auxo g `(,h ,r (,q ,z ,y)) t))
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
    (!-auxo g `(,h ,r (,q ,z ,y)) t))
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
	(!-auxo '() `(,la (,f) ,b) '(-> int int)))
      '(((lambda _.0 _.1) (num _.1) (sym _.0))))
    (equal?
     (run 1 (expr type)
       (fresh (h r q z y t u v)
         (== `(,h ,r (,q ,z ,y)) expr)
         (== `(,t ,u ,v) type)
         (!-auxo '() expr type)))
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
    (!-auxo '()
        '(let ((d (lambda (f) (lambda (x) (f (f x))))))
           (let ((a1 ((d (lambda (x) (sub1 x))) 2)))
             (let ((a2 ((d (lambda (x) (if x #t #f))) #t)))
               d)))
        q))
  '((-> (-> _.0 _.0) (-> _.0 _.0))))

(test 'w-let-poly-2
  (run* (q) 
    (!-auxo '()
        '(let ((d (lambda (f) (lambda (x) (f (f x))))))
           (let ((a1 ((d (lambda (x) (sub1 x))) 2)))
             (let ((a2 (d (lambda (x) (if x #t #f)))))
               a1)))
        q))
  '(int))

(test 'w-let-poly-3
  (run* (q) 
    (!-auxo '()
        '(let ((d (lambda (f) (lambda (x) (f (f x))))))
           (let ((a1 ((d (lambda (x) (sub1 x))) 2)))
             (let ((a2 (d (lambda (x) (if x #t #f)))))
               a2)))
        q))
  '((-> bool bool)))

(test 'w-let-poly-4
  (run* (q) 
    (!-auxo '()
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
    (!-auxo '()
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



(test 'polymorphic-let-monomorphic
  (run* (q)
    (!-auxo '()
        '((lambda (g)
            (let ((f (lambda (x) (g x))))
              (if (f (zero? 5))
                  (+ (f 4) 8)
                  (+ (f 3) 7))))
          (lambda (y) y))
        q))
  '(???))
