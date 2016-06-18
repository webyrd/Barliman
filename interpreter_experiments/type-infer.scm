;;; (Obsolete -- instead, see 'kanren-type-type-inference.scm')



;;; WEB -- 16 June 2016
;;;
;;; This is a modified version of Oleg Kiselyov's type inferencer at
;;; http://kanren.cvs.sourceforge.net/kanren/kanren/mini/type-inference.scm




; Hindley-Milner type inference and type population _relation_
;
; This is a _pure_ relation that relates a term and its type. It can
; be used for type inference (determining the type for a term), type
; checking (making sure that a term is of the given type), and term
; reconstruction (constructing a term that has the desired type).  We
; may also specify a part of a term and a part of a type, and ask the
; system to fill in the rest. In the latter applications, this code
; acts as a theorem prover in intuitionistic logic.
;
; When generating a term for a type, we may ask for terms in normal form
; only. We fully support polymorphic types and let-polymorphism.
; 
; The end of the file shows the applications of the type reconstruction
; to deriving CPS terms for call/cc, shift, and reset from their types.
;
; This code is a re-write in mini-Kanren of the type checker in the full
; Kanren: ../examples/type-inference.scm (version 4.50 2005/02/12)
; We use only the second approach from that file.
;
; The term language is Scheme: integers, booleans, and pairs (aka products)
; are supported, along with a sum data type:
;  constructors are (inl X), (inr X), and the deconstructor is
;  (either (var Exp) on-left on-right)
;
; The internal term language is similar, with the explicit tags for
; integer and boolean literals.
; The type language is infix, with constants int, bool
; and _infix_ constructors ->, *, +
; The constructor -> is _right_ associative.
;

; Future plans: make sure that in the generation phase, all given
; variables are used (or used only once, etc). So, we can generate
; _or_ typecheck terms using uniqueness, linearity, etc. constraints.
; Regarding linearity: the `if' form has to be handled carefully,
; as its two branches are `parallel'.
; Add call/cc or abort as a primitive, and try to generate some formulas
; from classical logic.

; $Id: type-inference.scm,v 1.8 2006/01/10 09:44:48 oleg Exp $


; The Unit testing framework
(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (test-check title tested-expression expected-result #t))
    ((_ title tested-expression expected-result show-flag)
     (begin
       (printf "~s..." title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (if (equal? expected produced)
             (printf  " works!\n")
             (printf "Failed ~s: ~a\nExpected: ~a\nComputed: ~a\n"
                     title 'tested-expression expected produced)))))))

; Unlike products -- car, cdr, cons -- sums are not standard in Scheme.
; The following is their simple implementation 
(define (inl x) (vector #f x))
(define (inr x) (vector #t x))
(define-syntax either
  (syntax-rules ()
    ((either (v exp) onl onr)
     (let* ((x exp)
	    (v (vector-ref x 1)))
       (if (vector-ref x 0) onr onl)))))


; We use a subset of Scheme itself as the source language
; The following two functions translate between the source language
; and the internal term language. 
; NB! In the term language, all bound variables must be unique. So
; this function should also do alpha-renaming.
; The function 'parse' is somewhat similar to the syntax-rule
; processor in that both produce AST from the surface language -- and both
; annotate identifiers so that the names of all bound identifiers are unique.

(define parse
  (lambda (term) (parse-env term '())))

(define (parse-env e env)
  (define (fmap e) (cons (car e) (map (lambda (t) (parse-env t env)) (cdr e))))
  ; extend the env and support renaming. We rename the identifier
  ; only in the case of a conflict.
  ; the algorithm below is very dumb (but short)
  (define (env-ext v env)
    (if (not (assq v env)) (cons (cons v v) env) ; identity mapping
      (let loop ((cnt 1))
	(let ((new-v 
		(string->symbol
		  (string-append (symbol->string v) (number->string cnt)))))
	  (if (assq new-v env) (loop (+ 1 cnt))
	    (cons (cons v new-v) env))))))
    (cond
      ((symbol? e) `(var ,(cdr (assq e env)))); support alpha-renaming
      ((number? e) `(intc ,e))
      ((boolean? e) `(boolc ,e))
      (else 
	(case (car e)
	  ((zero? sub1 + if cons car cdr inl inr fix)
	    (fmap e))
	  ((lambda)
	    (let* ((old-v (caadr e))
		   (new-env (env-ext old-v env))
		   (new-v (cdr (assq old-v new-env))))
	      `(lambda (,new-v) ,(parse-env (caddr e) new-env))))
	  ((let) 
	    (let* ((old-v (caar (cadr e)))
		   (new-env (env-ext old-v env))
		   (new-v (cdr (assq old-v new-env))))
	      `(let ((,new-v ,(parse-env (cadar (cadr e)) env)))
		 ,(parse-env (caddr e) new-env))))
	  ((either) 
	    (let* ((old-v (car (cadr e)))
		   (new-env (env-ext old-v env))
		   (new-v (cdr (assq old-v new-env))))
	      `(either (,new-v ,(parse-env (cadr (cadr e)) env))
		 ,(parse-env (caddr e) new-env)
		 ,(parse-env (cadddr e) new-env))))
	  (else (fmap (cons 'app e)))))))


(define (unparse e)
  (define (fmap e) (cons (car e) (map unparse (cdr e))))
    (case (car e)
      ((var) (cadr e))
      ((intc) (cadr e))
      ((boolc) (cadr e))
      ((zero? sub1 + if cons car cdr inl inr fix) (fmap e))
      ((lambda) `(lambda (,(car (cadr e))) ,(unparse (caddr e))))
      ((let) 
       `(let ((,(car (car (cadr e)))
               ,(unparse (cadr (car (cadr e))))))
          ,(unparse (caddr e))))
      ((either) 
       `(either (,(car (cadr e))
		 ,(unparse (cadr (cadr e))))
          ,(unparse (caddr e)) ,(unparse (cadddr e))))
      ((app) (cdr (fmap e)))))


; We define a first-class (and recursive) relation !-
; so that (!- `(var ,v) t) holds iff the source term variable v has a type
; t. 
; This variant is close to the `natural deduction' scheme.
; It also has an OO flavor: we need open recursion.

; The following are the separate components of which the relation
; !- will be built. All these components nevertheless receive the full
; !- as the argument. Actually, they will receive the 'self'-like
; argument. We need to explicitly find the fixpoint.

; Integer and boolean constants
(define int-rel
  (lambda (s!-)
    (lambda (e t)
      (fresh (x)
	(== e `(intc ,x))
	(== t 'int)))))

(define bool-rel
  (lambda (s!-)
    (lambda (e t)
      (fresh (x)
	(== e `(boolc ,x))
	(== t 'bool)))))


; types of primitive operations

; (zero? x)

(define zero?-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (x)
	  (== e `(zero? ,x))		; the term
	  (== t 'bool)			; the type
	  (!- x 'int))))))		; provided that x is int

(define sub1-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (x)
	  (== e `(sub1 ,x))		; the term
	  (== t 'int)			; the type
	  (!- x 'int))))))		; provided that x is int

(define +-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (x y)
	  (== e `(+ ,x ,y))		; the term
	  (== t 'int)			; the type
	  (!- x 'int)  (!- y 'int))))))	; provided that x,y are int

; products
(define cons-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (x y tx ty)
	  (== e `(cons ,x ,y))		; the term
	  (== t `(,tx * ,ty))		; the type
	  (!- x tx)  (!- y ty))))))	; provided that x,y are typeable

(define car-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (x t2)
	  (== e `(car ,x))		; the term
	  (!- x `(,t * ,t2)))))))	; provided that x is a product type

(define cdr-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (x t2)
	  (== e `(cdr ,x))		; the term
	  (!- x `(,t2 * ,t)))))))	; provided that x is a product type
; sums
(define inl-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (x tl tr)
	  (== e `(inl ,x))		; the term
	  (== t `(,tl + ,tr))		; the type
	  (!- x tl))))))		; provided that x is typeable

(define inr-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (x tl tr)
	  (== e `(inr ,x))		; the term
	  (== t `(,tl + ,tr))		; the type
	  (!- x tr))))))		; provided that x is typeable


; conditionals

(define if-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (test conseq alt)
	  (== e `(if ,test ,conseq ,alt))		; the term
	  (!- test 'bool)		; provided that test is bool
	  (!- conseq t)
	  (!- alt t))))))		; and conseq, alt are of the same type


; Abstraction, application, fixpoint, and other binding forms

; Here we extend !- with an additional assumption that v has the type
; type-v. This extension corresponds to a non-generic, regular type.
(define lambda-rel
  (lambda (s!-)
    (lambda (e t)
      (fresh (v tb body type-v)
	(== e `(lambda (,v) ,body))
	(== t `(,type-v -> . ,tb))
	(let* ((snew-!-
		 (lambda (self)
		   (lambda (e t)
		     (conde ; lexically-scoped relation
		       ((== e `(var ,v)) (== t type-v))
		       (((s!- self) e t))))))
		(!- (snew-!- snew-!-)))
	  (!- body tb))))))		; check body in so extended env

; This is also a binding form
(define either-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (v x onl onr tl tr)
	  (== e `(either (,v ,x) ,onl ,onr)) ; the term
	  (!- x `(,tl + ,tr))		; the sum type
	  ; if onl is evaluated, v has the type tl
	  ; this is similar to GADT!
	  (let* ((snew-!-
		 (lambda (self)
		   (lambda (e t)
		     (conde ; lexically-scoped relation
		       ((== e `(var ,v)) (== t tl))
		       (((s!- self) e t))))))
		(!- (snew-!- snew-!-)))
	    (!- onl t))
	  ; if onr is evaluated, v has the type tr
	  (let* ((snew-!-
		 (lambda (self)
		   (lambda (e t)
		     (conde ; lexically-scoped relation
		       ((== e `(var ,v)) (== t tr))
		       (((s!- self) e t))))))
		(!- (snew-!- snew-!-)))
	    (!- onr t))
	  )))))

(define app-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (t-rand rand rator)
	  (== e `(app ,rator ,rand))
	  (!- rator `(,t-rand -> . ,t))
	  (!- rand t-rand))))))

(define fix-rel
  (lambda (s!-)
     (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (rand)
	  (== e `(fix ,rand))
	  (!- rand `(,t -> . ,t)))))))

; Let with let-polymorphism
; The reason to test `(!- g rand some-type)' at the very beginning is
; to make sure that `rand' itself is well-typed. As Ken pointed out,
; we must outlaw expressions such as (let ((x (z z))) y) where 'x'
; does not occur in the body. The variable 'x' still must have some
; type.

(define polylet-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (v rand body)
	  (== e `(let ((,v ,rand)) ,body))
	  (fresh (some-type) (!- rand some-type))
	  (let* ((snew-!-
		   (lambda (self)
		     (lambda (e t)
		       (conde
			 ((== e `(var ,v)) (!- rand t))
			 (((s!- self) e t))))))
		  (!- (snew-!- snew-!-)))
	    (!- body t)))))))

; Now we build the recursive !- relation, as a fixpoint

(define s!-
  (lambda (self)
    (lambda (e t)
      (conde
	(((int-rel self) e t))
	(((bool-rel self) e t))
	(((zero?-rel self) e t))
	(((sub1-rel self) e t))
	(((+-rel self) e t))
	(((cons-rel self) e t))
	(((car-rel self) e t))
	(((cdr-rel self) e t))
	(((inl-rel self) e t))
	(((inr-rel self) e t))
 	(((either-rel self) e t))
	(((if-rel self) e t))
	(((lambda-rel self) e t))
	(((app-rel self) e t))
	(((fix-rel self) e t))
	(((polylet-rel self) e t))
	))))
	
(define !- (s!- s!-))


;------------------------------------------------------------------------
;			tests


(test-check 'test-!-1
  (run* (q) (!- '(intc 17) 'int))
  '(_.0))

(test-check 'test-!-2
  (run* (q) (!- '(intc 17) q))
  '(int))

(test-check 'test-primitives
  (run* (q) (!- '(zero? (intc 24)) q))
  '(bool))

(test-check 'test-sub1
  (run* (q) (!- '(zero? (sub1 (intc 24))) q))
  '(bool))

(test-check 'test-+
  (run* (q) (!- '(zero? (sub1 (+ (intc 18) (+ (intc 24) (intc 50))))) q))
  '(bool))

(test-check 'test-if
  (run* (q) (!- '(if (zero? (intc 24)) (zero? (intc 3)) (zero? (intc 4))) q))
  '(bool))

(test-check 'test-if-2
  (run* (q) 
    (!- '(if (if (zero? (intc 1)) (boolc #t) (boolc #f))
                     (intc 0)
                     (+ (intc 1) (intc 2)))
      q))
  '(int))

(test-check 'test-parse
  (parse '(lambda (x) (lambda (x) x)))
  '(lambda (x) (lambda (x1) (var x1))))


(test-check 'variables-4a
  (run* (q) (!- '(lambda (x) (+ (var x) (intc 5))) q))
  '((int -> . int)))


(test-check 'variables-4c
  (run* (q) (!- '(lambda (a) (lambda (x) (+ (var x) (var a)))) q))
  '((int -> int -> . int)))


(test-check 'variables-product
  (run* (q) (!- (parse '(lambda (a) (cdr (car (cons a 1))))) q))
  '(((_.0 * _.1) -> . _.1)))

(test-check 'variables-sum
  (run* (q) (!- 
	      (parse 
		'(lambda (x) (either (v x) (inl (car v)) (inr (cdr v)))))
	      q))
  '((((_.0 * _.1) + (_.2 * _.3)) -> . (_.0 + _.3))))

(test-check 'everything-but-polymorphic-let-1
  (run* (q) 
    (!- (parse
	  '(lambda (f)
	     (lambda (x)
	       ((f x) x))))
      q))
  '(((_.0 -> _.0 -> . _.1) -> _.0 -> . _.1)))


(test-check 'everything-but-polymorphic-let-2
  (run* (q) 
    (!- (parse
          '((fix (lambda (sum)
                   (lambda (n)
                     (+ n (sum (sub1 n))))))
            10))
      q))
  '(int))

; The following should not typecheck because lambda-binding is not polymorphic
(test-check 'everything-but-polymorphic-let-3
  (run* (q) 
    (!- (parse '((lambda (f)
                   (if (f (zero? 5))
                       (+ (f 4) 8)
                       (+ (f 3) 7)))
                 (lambda (x) x)))
      q))
  '())

; But if we use let, with its let-polymorphis, it works.
(test-check 'polymorphic-let-1
  (run* (q) 
    (!- (parse
          '(let ((f (lambda (x) x)))
             (if (f (zero? 5))
                 (+ (f 4) 8)
                 (+ (f 3) 7))))
      q))
  '(int))


(test-check 'with-robust-syntax
  (run* (q) 
    (!- '(app
           (fix
             (lambda (sum)
               (lambda (n)
                 (if (if (zero? (var n)) (boolc #t) (boolc #f))
                     (intc 0)
                     (+ (var n) (app (var sum) (sub1 (var n))))))))
           (intc 10))
      q))
  '(int))

(test-check "test19"
  (run* (q)
    (!- (parse '((fix (lambda (sum)
                        (lambda (n)
                          (+ n (sum (sub1 n))))))
                 10))
        q))
  '(int))


; Generating a term for a type
(test-check 'type-habitation-1
  (run 10 (q) (!- q 'int))
  '((intc _.0)
    (sub1 (intc _.0))
    (sub1 (sub1 (intc _.0)))
    (+ (intc _.0) (intc _.1))
    (sub1 (sub1 (sub1 (intc _.0))))
    (sub1 (+ (intc _.0) (intc _.1)))
    (+ (intc _.0) (sub1 (intc _.1)))
    (+ (sub1 (intc _.0)) (intc _.1))
    (sub1 (sub1 (sub1 (sub1 (intc _.0)))))
    (sub1 (sub1 (+ (intc _.0) (intc _.1)))))
)

(test-check 'type-habitation-2
  (run 5 (q) (!- q '(int -> . int)))
  '((lambda (_.0) (var _.0))
    (lambda (_.0) (intc _.1))
    (lambda (_.0) (sub1 (var _.0)))
    (car (cons (lambda (_.0) (var _.0)) (intc _.1)))
    (car (cons (lambda (_.0) (var _.0)) (boolc _.1))))
  )


; Note the constants 'a rather than logical variables a:
; 'a is an eigne-value. We want to have the polymorphic type
(test-check 'type-habitation-3
  (run 10 (q) (!- q `((a -> . a) -> . (a -> . a))))
  '((lambda (_.0) (var _.0))
    (car (cons (lambda (_.0) (var _.0)) (intc _.1)))
    (car (cons (lambda (_.0) (var _.0)) (boolc _.1)))
    (car (cons (lambda (_.0) (var _.0)) (zero? (intc _.1))))
    (car (cons (lambda (_.0) (var _.0)) (sub1 (intc _.1))))
    (car (cons
          (lambda (_.0) (var _.0))
          (zero? (sub1 (intc _.1)))))
    (car (cons
          (lambda (_.0) (var _.0))
          (sub1 (sub1 (intc _.1)))))
    (car (cons
          (lambda (_.0) (var _.0))
          (zero? (sub1 (sub1 (intc _.1))))))
    (car (cons
          (lambda (_.0) (var _.0))
          (+ (intc _.1) (intc _.2))))
    (car (cons
          (lambda (_.0) (var _.0))
          (zero? (+ (intc _.1) (intc _.2))))))
  )

(test-check 'type-habitation-4
  (run 10 (q) 
    (fresh (_) (== q `(lambda . ,_)) (!- q `((a -> . a) -> . (a -> . a)))))
  '((lambda (_.0) (var _.0)) (lambda (_.0) (car (cons (var _.0) (var _.0))))
    (lambda (_.0) (car (cons (var _.0) (intc _.1))))
    (lambda (_.0) (cdr (cons (var _.0) (var _.0))))
    (lambda (_.0) (car (cons (var _.0) (boolc _.1))))
    (lambda (_.0) (lambda (_.1) (var _.1)))
    (lambda (_.0) (cdr (cons (intc _.1) (var _.0))))
    (lambda (_.0) (car (cons (var _.0) (zero? (intc _.1)))))
    (lambda (_.0) (if (boolc _.1) (var _.0) (var _.0)))
    (lambda (_.0) (cdr (cons (boolc _.1) (var _.0)))))
  )

; If we wish to find only combinators, we can tell the system what
; kind of terms to use

(define-syntax make-!
  (syntax-rules ()
    ((_ rel ...)
     ((lambda (x) (x x))
      (lambda (self)
	(lambda (e t)
	  (conde
	    (((rel self) e t)) ...
	    )))))))

(define c!- (make-! lambda-rel app-rel))


(time
  (map unparse
    (run 10 (q) 
      (fresh (_) (== q `(lambda . ,_))
	(c!- q `((a -> . a) -> . (a -> . a)))))))

(printf "\nSome examples from Djinn: inferring morphisms of the continuation
monad\n")
; Some examples from Djinn: deriving the type of return and call/cc
; in the continuation monad:

;    Djinn> returnC ? a -> C a
;    returnC :: a -> C a
;    returnC x1 x2 = x2 x1

(define (cont a) `((,a -> . r) -> . r))
(display (map unparse (run 1 (q)  (c!- q `(a -> . ,(cont 'a))))))
(newline)

;    Djinn> bindC ? C a -> (a -> C b) -> C b
;    bindC :: C a -> (a -> C b) -> C b
;    bindC x1 x2 x3 = x1 (\ c15 -> x2 c15 (\ c17 -> x3 c17))

; Deriving the expression for call/cc and bind is really difficult. So,
; we restrict the app-rel to avoid the redexes. We don't want to generate
; terms with redexes anyway...
; The above prevents call-by-name redexes. We may wish to exclude only CBV
; redexes (lambdas and variables in the operand position). It is interesting
; how it changes the result...
(define appn-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (lambda (e t)
	(fresh (t-rand rand rator)
	  (== e `(app ,rator ,rand))
	  (fresh (_) (conde ((== rator `(var ,_))) 
		            ((== rator `(app . ,_)))))
	  (!- rator `(,t-rand -> . ,t))
	  (!- rand t-rand))))))
(define c!- (make-! lambda-rel appn-rel))

(printf "\nbind\n")
(printf "~s\n"
(time 
  (map unparse 
    (run 1 (q) 
     (fresh (x1 x2 _) (== q `(lambda (,x1) (lambda (,x2) . ,_)))
      (c!- q `(,(cont 'a) -> . ((a -> . ,(cont 'b)) -> . ,(cont 'b))))))))
)

;    Djinn> type C a = (a -> r) -> r
;    Djinn> callCC ? ((a -> C b) -> C a) -> C a
;   callCC x1 x2 = x1 (\ c15 _ -> x2 c15) (\ c11 -> x2 c11)

(printf "\ncall/cc\n")
(printf "~s\n"
 (time 
  (map unparse 
    (run 1 (q) 
     (fresh (x1 x2 _) (== q `(lambda (,x1) (lambda (,x2) . ,_)))
      (c!- q `(((a -> . ,(cont 'b)) -> . ,(cont 'a)) -> . ,(cont 'a)))))))
)


(printf "\nInferring the expressions for shift and reset\n")

(define (cont a r) `((,a -> . ,r) -> . ,r))

(printf "\nreset\n")
(printf "~s\n"
 (time 
  (map unparse 
    (run 1 (q) 
     (fresh (x1 x2 _) (== q `(lambda (,x1) (lambda (,x2) . ,_)))
      (c!- q `(,(cont 'a 'a) -> . ,(cont 'a 'r)))))))
)

(printf "\nshift\n")
(printf "~s\n"
 (time 
  (map unparse 
    (run 1 (q) 
     (fresh (x1 x2 _) (== q `(lambda (,x1) (lambda (,x2) . ,_)))
      (c!- q `(((a -> . ,(cont 'b 'r)) -> . ,(cont 'b 'b)) 
		-> . ,(cont 'a 'b)))))))
)




;;; Will's tests

;;; adapted from:
;;;
;;; Polymorphic Types
;;; and Type Inference Programming Languages CS442
;;; David Toman
;;; School of Computer Science University of Waterloo
;;;
;;; https://cs.uwaterloo.ca/~david/cs442/lect-TYPEINFERENCE.pdf
(test-check 'w-let-poly-1
  (run* (q) 
    (!- (parse
         '(let ((d (lambda (f) (lambda (x) (f (f x))))))
            (let ((a1 ((d (lambda (x) (sub1 x))) 2)))
              (let ((a2 ((d (lambda (x) (if x #t #f))) #t)))
                d))))
        q))
  '(((_.0 -> . _.0) -> _.0 -> . _.0)))

(test-check 'w-let-poly-2
  (run* (q) 
    (!- (parse
         '(let ((d (lambda (f) (lambda (x) (f (f x))))))
            (let ((a1 ((d (lambda (x) (sub1 x))) 2)))
              (let ((a2 (d (lambda (x) (if x #t #f)))))
                a1))))
        q))
  '(int))

(test-check 'w-let-poly-3
  (run* (q) 
    (!- (parse
         '(let ((d (lambda (f) (lambda (x) (f (f x))))))
            (let ((a1 ((d (lambda (x) (sub1 x))) 2)))
              (let ((a2 (d (lambda (x) (if x #t #f)))))
                a2))))
        q))
  '((bool -> . bool)))

(test-check 'w-let-poly-4
  (run* (q) 
    (!- (parse
         '(let ((d (lambda (f) (lambda (x) (f (f x))))))
            (let ((a1 ((d (lambda (x) (sub1 x))) 2)))
              (let ((a2 ((d (lambda (x) (if x #t #f))) #t)))
                a2))))
        q))
  '(bool))


#|
(test-check 'w-length-1
  (run* (q) 
    (!- '(app
           (fix
             (lambda (length)
               (lambda (l)
                 (if (if (null? (var l)) (boolc #t) (boolc #f))
                     (intc 0)
                     (+ 1 (app (var length) (cdr (var l))))))))
           (cons (intc 1) (cons (intc 2) (cons (intc 3)))))
      q))
  '(int))
|#
