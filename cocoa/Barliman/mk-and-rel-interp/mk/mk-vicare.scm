; This file needs to be loaded before mk.scm for Vicare. I can't figure
; out how to do loads relative to a source file rather than the working
; directory, else this file would load mk.scm.


; Trie implementation, due to Abdulaziz Ghuloum. Used for substitution
; and constraint store.

  ;;; subst ::= (empty)
  ;;;         | (node even odd)
  ;;;         | (data idx val)

(define-record-type node (fields e o))

(define-record-type data (fields idx val))

(define shift (lambda (n) (fxsra n 1)))

(define unshift (lambda (n i) (fx+ (fxsll n 1) i)))

;;; interface

(define t:size
  (lambda (x) (t:aux:size x)))

(define t:bind
  (lambda (xi v s)
    (unless (and (fixnum? xi) (>= xi 0))
      (error 't:bind "index must be a fixnum, got ~s" xi))
    (t:aux:bind xi v s)))

(define t:unbind
  (lambda (xi s)
    (unless (and (fixnum? xi) (>= xi 0))
      (error 't:unbind "index must be a fixnum, got ~s" xi))
    (t:aux:unbind xi s)))

(define t:lookup
  (lambda (xi s)
    (unless (and (fixnum? xi) (>= xi 0))
      (error 't:lookup "index must be a fixnum, got ~s" xi))
    (t:aux:lookup xi s)))

(define t:binding-value
  (lambda (s)
    (unless (data? s)
      (error 't:binding-value "not a binding ~s" s))
    (data-val s)))

;;; helpers

(define t:aux:push
  (lambda (xi vi xj vj)
    (if (fxeven? xi)
        (if (fxeven? xj)
            (make-node (t:aux:push (shift xi) vi (shift xj) vj) '())
            (make-node (make-data (shift xi) vi) (make-data (shift xj) vj)))
        (if (fxeven? xj)
            (make-node (make-data (shift xj) vj) (make-data (shift xi) vi))
            (make-node '() (t:aux:push (shift xi) vi (shift xj) vj))))))

(define t:aux:bind
  (lambda (xi vi s*)
    (cond
      [(node? s*)
       (if (fxeven? xi)
           (make-node (t:aux:bind (shift xi) vi (node-e s*)) (node-o s*))
           (make-node (node-e s*) (t:aux:bind (shift xi) vi (node-o s*))))]
      [(data? s*)
       (let ([xj (data-idx s*)] [vj (data-val s*)])
         (if (fx= xi xj)
             (make-data xi vi)
             (t:aux:push xi vi xj vj)))]
      [else (make-data xi vi)])))

(define t:aux:lookup
  (lambda (xi s*)
    (cond
      [(node? s*)
       (if (fxeven? xi)
           (t:aux:lookup (shift xi) (node-e s*))
           (t:aux:lookup (shift xi) (node-o s*)))]
      [(data? s*)
       (if (fx= (data-idx s*) xi)
           s*
           #f)]
      [else #f])))

(define t:aux:size
  (lambda (s*)
    (cond
      [(node? s*) (fx+ (t:aux:size (node-e s*)) (t:aux:size (node-o s*)))]
      [(data? s*) 1]
      [else 0])))

(define t:aux:cons^
  (lambda (e o)
    (cond
      [(or (node? e) (node? o)) (make-node e o)]
      [(data? e)
       (make-data (unshift (data-idx e) 0) (data-val e))]
      [(data? o)
       (make-data (unshift (data-idx o) 1) (data-val o))]
      [else '()])))

(define t:aux:unbind
  (lambda (xi s*)
    (cond
      [(node? s*)
       (if (fxeven? xi)
           (t:aux:cons^ (t:aux:unbind (shift xi) (node-e s*)) (node-o s*))
           (t:aux:cons^ (node-e s*) (t:aux:unbind (shift xi) (node-o s*))))]
      [(and (data? s*) (fx= (data-idx s*) xi)) '()]
      [else s*])))


; Substitution representation

(define empty-subst-map '())

(define subst-map-length t:size)

; Returns #f if not found, or a pair of u and the result of the lookup.
; This distinguishes between #f indicating absence and being the result.
(define subst-map-lookup
  (lambda (u S)
    (let ((res (t:lookup (var-idx u) S)))
      (if res
        (data-val res)
        unbound))))

(define (subst-map-add S var val)
  (t:bind (var-idx var) val S))

(define subst-map-eq? eq?)


; Alternative (unused) substitution representation, using alists.
; Performance with the tries is usually about the same and
; can be much better for huge substitutions.

#|
(define empty-subst-map '())

(define subst-map-length length)

; Returns #f if not found, or a pair of u and the result of the lookup.
; This distinguishes between #f indicating absence and being the result.
(define subst-map-lookup
  (lambda (u S)
    (let ((res (assq u S)))
      (if res
        (cdr res)
        unbound))))

(define (subst-map-add S var val)
  (cons (cons var val) S))

(define subst-map-eq? eq?)
|#


; Constraint store representation

(define empty-C '())

(define set-c
  (lambda (v c st)
    (state (state-S st)
           (t:bind (var-idx v) c (state-C st))
           (state-depth st)
           (state-deferred st))))

(define lookup-c
  (lambda (v st)
    (let ((res (t:lookup (var-idx v) (state-C st))))
      (if res
        (data-val res)
        empty-c))))

; t:unbind either is buggy or doesn't do what I would expect, so
; I implement remove by setting the value to the empty constraint record.
(define remove-c
  (lambda (v st)
    (let ((res (t:bind (var-idx v) empty-c (state-C st))))
      (state (state-S st) res (state-depth st) (state-deferred st)))))


; Misc. missing functions

(define (remove-duplicates l)
  (cond ((null? l)
         '())
        ((member (car l) (cdr l))
         (remove-duplicates (cdr l)))
        (else
         (cons (car l) (remove-duplicates (cdr l))))))

(define (foldl f init seq)
  (if (null? seq)
    init
    (foldl f
           (f (car seq) init)
           (cdr seq))))

