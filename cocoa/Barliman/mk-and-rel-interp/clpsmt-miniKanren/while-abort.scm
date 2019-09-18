;;; The following example is adapted from:
;;;
;;; https://github.com/webyrd/polyconf-2015/blob/master/talk-code/while-interpreter/while-abort.scm


;; Relational interpreter for the While language, extended with 'abort'.

;; Based on the While language description in 'Semantics with
;; Applications' by Nielson and Nielson.  Here we use the 1999 revised
;; edition of the book, available online at http://www.daimi.au.dk/~hrn.
;; The original edition was published in 1992 by John Wiley & Sons.



;; Arithmetic expression language (Table 1.1 on p.13 of the book)


;;; Important:  all variables are bound to zero by default
(define store-lookupo
  (lambda (x s val)
    (fresh ()
      (symbolo x)
      (conde
        [(== '() s)
         (== 0 val)]
        [(fresh (y v rest)
           (== `((,y . ,v) . ,rest) s)
           (conde
             [(== x y) (== v val)]
             [(=/= x y) (symbolo y) (store-lookupo x rest val)]))]))))

(define not-in-storeo
  (lambda (x s)
    (fresh ()
      (symbolo x)
      (conde
        [(== '() s)]
        [(fresh (y _ s^)
           (== `((,y . ,_) . ,s^) s)
           (=/= y x)
           (symbolo y)
           (not-in-storeo x s^))]))))

(define update-storeo
  (lambda (x v s s-out)
    (fresh ()
      (symbolo x)
      (conde
        [(replaceo x v s s-out)]
        [(== `((,x . ,v) . ,s) s-out)
         (not-in-storeo x s)]))))

(define replaceo
  (lambda (x val s s-out)
    (fresh (y v rest s^)
      (== `((,y . ,v) . ,rest) s)
      (symbolo x)
      (conde
        [(== x y)
         (== `((,x . ,val) . ,rest) s-out)]
        [(=/= x y)
         (== (cons `(,y . ,v) s^) s-out)
         (symbolo y)
         (replaceo x val rest s^)]))))



(define Ao
  (lambda (expr s num)
    (conde
      
      [(symbolo expr) (store-lookupo expr s num)]

      ;; Perhaps should use (z/assert `(= ,expr ,num)) instead of (== expr num)
      [(numbero expr) (== expr num)]

      [(fresh (a1 a2 n1 n2)
         (== `(+ ,a1 ,a2) expr)
         (z/assert `(= ,num (+ ,n1 ,n2)))
         (Ao a1 s n1)
         (Ao a2 s n2))]

      [(fresh (a1 a2 n1 n2)
         (== `(- ,a1 ,a2) expr)
         (z/assert `(= ,num (- ,n1 ,n2)))
         (Ao a1 s n1)
         (Ao a2 s n2))]

      [(fresh (a1 a2 n1 n2)
         (== `(* ,a1 ,a2) expr)
         (z/assert `(= ,num (* ,n1 ,n2)))
         (Ao a1 s n1)
         (Ao a2 s n2))]
      
      )))

;; Boolean expression language (Table 1.2 on p.14 of the book)

(define Bo
  (lambda (expr s val)
    (conde
      [(== 'true expr)
       (== 'tt val)]
      [(== 'false expr)
       (== 'ff val)]
      
      [(fresh (b v)
         (== `(not ,b) expr)
         (conde
           [(== 'tt val) (== 'ff v)]
           [(== 'ff val) (== 'tt v)])
         (Bo b s v))]
      
      [(fresh (b1 b2 v1 v2)
         (== `(and ,b1 ,b2) expr)
         (Bo b1 s v1)
         (Bo b2 s v2)
         (conde
           [(== 'tt val) (== 'tt v1) (== 'tt v2)]
           [(== 'ff val)
            (conde
              [(== 'ff v1)]
              [(== 'tt v1) (== 'ff v2)])]))]
      
      [(fresh (a1 a2 n1 n2)
         (== `(= ,a1 ,a2) expr)
         (conde
           [(== 'tt val) (z/assert `(= ,n1 ,n2))]
           [(== 'ff val) (z/assert `(not (= ,n1 ,n2)))])         
         (Ao a1 s n1)
         (Ao a2 s n2))]

      [(fresh (a1 a2 n1 n2)
         (== `(!= ,a1 ,a2) expr)
         (conde
           [(== 'tt val) (z/assert `(not (= ,n1 ,n2)))]
           [(== 'ff val) (z/assert `(= ,n1 ,n2))])         
         (Ao a1 s n1)
         (Ao a2 s n2))]

      [(fresh (a1 a2 n1 n2)
         (== `(< ,a1 ,a2) expr)
         (conde
           [(== 'tt val) (z/assert `(< ,n1 ,n2))]
           [(== 'ff val) (z/assert `(<= ,n2 ,n1))])
         (Ao a1 s n1)         
         (Ao a2 s n2))]
      
      [(fresh (a1 a2 n1 n2)
         (== `(<= ,a1 ,a2) expr)
         (conde
           [(== 'tt val) (z/assert `(<= ,n1 ,n2))]
           [(== 'ff val) (z/assert `(< ,n2 ,n1))])
         (Ao a1 s n1)
         (Ao a2 s n2))]

      )))

;; Natural semantics for While (Table 2.1 on p.20 of the book)

(define with-abort-g
  (lambda (in out g)
    (fresh ()
      (conde
        [(absento 'abort in)
         (g in out)]
        [(fresh (s)
           (== `(abort ,s) in)
           (== in out))]))))

(define-syntax with-abort
  (syntax-rules ()
    [(_ in out body body* ...)
     (with-abort-g in out (lambda (in out) (fresh () body body* ...)))]))

(define ->o
  (lambda (config out)
    (conde
      [(fresh (s)
         (== `((skip) ,s) config)
         (== s out))]

      [(fresh (s)
         (== `((abort) ,s) config)
         (== `(abort ,s) out))]
      
      [(fresh (x a v s s^)
         (== `((:= ,x ,a) ,s) config)
         (== s^ out)
         (symbolo x)
         (Ao a s v)
         (update-storeo x v s s^))]

      [(fresh (b bv S1 S2 s)
         (== `((if ,b ,S1 ,S2) ,s) config)
         (Bo b s bv)
         (conde
           [(== 'tt bv)
            (->o `(,S1 ,s) out)]
           [(== 'ff bv)
            (->o `(,S2 ,s) out)]))]
      
      [(fresh (S1 S2 s s^)
         (== `((seq ,S1 ,S2) ,s) config)         
         (->o `(,S1 ,s) s^)
         (with-abort s^ out
           (->o `(,S2 ,s^) out)))]

      [(fresh (b bv S s)
         (== `((while ,b ,S) ,s) config)
         (Bo b s bv)
         (conde
           [(== 'ff bv)
            (== s out)]
           [(== 'tt bv)
            (fresh (s^)
              (->o `(,S ,s) s^)
              (with-abort s^ out
                (->o `((while ,b ,S) ,s^) out)))]))]      

      )))

