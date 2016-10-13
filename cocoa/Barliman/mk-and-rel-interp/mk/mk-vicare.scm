; This file needs to be loaded before mk.scm for Vicare. I can't figure
; out how to do loads relative to a source file rather than the working
; directory, else this file would load mk.scm.


; Old trie implementation, due to Abdulaziz Ghuloum. Used for substitution
; and constraint store.

  ;;; subst ::= (empty)
  ;;;         | (node even odd)
  ;;;         | (data idx val)

(define-record-type node (fields e o))

(define-record-type data (fields idx val))

(define shift (lambda (n) (fxsra n 1)))

(define unshift (lambda (n i) (fx+ (fxsll n 1) i)))


;; Switched to N-way Trie implementation to reduce depth

(define shift-size 4)
(define node-size (fxsll 1 shift-size))
(define local-mask (fx- node-size 1))
(define (shift-n xi) (fxsra xi shift-size))
(define (local-n xi) (fxand xi local-mask))
(define node-n? vector?)
(define (node-n-new i0 v0)
  (define result (make-vector (fx+ i0 1) '()))
  (vector-set! result i0 v0)
  result)
(define (node-n-get nd idx)
  (if (fx<? idx (vector-length nd)) (vector-ref nd idx) '()))
(define (node-n-put nd idx val)
  (define len0 (vector-length nd))
  (define len1 (fxmax len0 (fx+ idx 1)))
  (define result (make-vector len1 '()))
  (let copy ((ci 0))
    (if (fx=? len0 ci) (begin (vector-set! result idx val) result)
      (begin (vector-set! result ci (vector-ref nd ci)) (copy (fx+ ci 1))))))

(define (nwt:size trie)
  (cond
    ((node-n? trie)
     (let loop ((ci 0) (sz 0))
       (if (fx=? node-size ci) sz
         (loop (fx+ ci 1) (fx+ sz (nwt:size (node-n-get trie ci)))))))
    ((data? trie) 1)
    (else 0)))

(define (nwt:lookup trie xi)
  (cond
    ((node-n? trie) (nwt:lookup (node-n-get trie (local-n xi)) (shift-n xi)))
    ((data? trie) (and (fx=? xi (data-idx trie)) trie))
    (else #f)))

(define (nwt:bind trie xi val)
  (cond
    ((node-n? trie)
     (let ((li (local-n xi)))
       (node-n-put trie li (nwt:bind (node-n-get trie li) (shift-n xi) val))))
    ((data? trie)
     (let ((xi0 (data-idx trie)))
       (if (fx=? xi0 xi) (make-data xi val)
         (nwt:bind (node-n-new (local-n xi0) (make-data (shift-n xi0) (data-val trie)))
                   xi val))))
    (else (make-data xi val))))

;;; n-way trie interface

(define t:size nwt:size)

(define t:bind
  (lambda (xi v s)
    (unless (and (fixnum? xi) (>= xi 0))
      (error 't:bind "index must be a fixnum, got ~s" xi))
    (nwt:bind s xi v)))

(define t:lookup
  (lambda (xi s)
    (unless (and (fixnum? xi) (>= xi 0))
      (error 't:lookup "index must be a fixnum, got ~s" xi))
    (nwt:lookup s xi)))

;;; old interface

;(define t:size
  ;(lambda (x) (t:aux:size x)))

;(define t:bind
  ;(lambda (xi v s)
    ;(unless (and (fixnum? xi) (>= xi 0))
      ;(error 't:bind "index must be a fixnum, got ~s" xi))
    ;(t:aux:bind xi v s)))

;(define t:unbind
  ;(lambda (xi s)
    ;(unless (and (fixnum? xi) (>= xi 0))
      ;(error 't:unbind "index must be a fixnum, got ~s" xi))
    ;(t:aux:unbind xi s)))

;(define t:lookup
  ;(lambda (xi s)
    ;(unless (and (fixnum? xi) (>= xi 0))
      ;(error 't:lookup "index must be a fixnum, got ~s" xi))
    ;(t:aux:lookup xi s)))

;;; interface

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

