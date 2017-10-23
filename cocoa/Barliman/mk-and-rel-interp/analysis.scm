;; TODO: remove loads after testing.
(load "mk/mk-vicare.scm")
(load "mk/mk.scm")

(define type-bottom #f)
(define type-top #t)
(define (type-repr procedure pair number? symbol? null? false? true?)
  (cond ((and procedure pair number? symbol? null? false? true?) type-top)
        ((or procedure pair number? symbol? null? false? true?)
         (vector procedure pair number? symbol? null? false? true?))
        (else type-bottom)))
(define (type-repr-procedure m) (vector-ref m 0))
(define (type-repr-pair m) (vector-ref m 1))
(define (type-repr-number? m) (vector-ref m 2))
(define (type-repr-symbol? m) (vector-ref m 3))
(define (type-repr-null? m) (vector-ref m 4))
(define (type-repr-false? m) (vector-ref m 5))
(define (type-repr-true? m) (vector-ref m 6))

;; TODO: use procedure details.
(define (type-procedure ins out) (type-repr #t #f #f #f #f #f #f))
(define (type-pair l r) (type-repr #f `(,l . ,r) #f #f #f #f #f))
(define type-procedure-top (type-procedure #f type-top))
(define type-pair-top (type-pair type-top type-top))
(define type-number (type-repr #f #f #t #f #f #f #f))
(define type-symbol (type-repr #f #f #f #t #f #f #f))
(define type-null (type-repr #f #f #f #f #t #f #f))
(define type-false (type-repr #f #f #f #f #f #t #f))
(define type-true (type-repr #f #f #f #f #f #f #t))

(define (type-complement t)
  (cond ((eq? type-top t) type-bottom)
        ((eq? type-bottom t) type-top)
        (else (type-repr
                (not (type-repr-procedure t))  ;; TODO: procedure details.
                (let ((pair-t (type-repr-pair t)))
                  (cond ((eq? #t pair-t) #f)
                        ((eq? #f pair-t) #t)
                        (else `(,(type-complement (car pair-t))
                                 . ,(type-complement (cdr pair-t))))))
                (not (type-repr-number? t))
                (not (type-repr-symbol? t))
                (not (type-repr-null? t))
                (not (type-repr-false? t))
                (not (type-repr-true? t))))))
(define (type<=? t u)
  (cond ((or (eq? type-top u) (eq? type-bottom t)) #t)
        ((or (eq? type-top t) (eq? type-bottom u)) #f)
        (else (let ((proc-t (type-repr-procedure t))
                    (proc-u (type-repr-procedure u))
                    (pair-t (type-repr-pair t))
                    (pair-u (type-repr-pair u)))
                (and (or (not proc-t) proc-u)  ;; TODO: procedure details.
                     (or (not pair-t)
                         (and pair-u (type<=? (car pair-t) (car pair-u))
                              (type<=? (cdr pair-t) (cdr pair-u))))
                     (or (not (type-repr-number? t)) (type-repr-number? u))
                     (or (not (type-repr-symbol? t)) (type-repr-symbol? u))
                     (or (not (type-repr-null? t)) (type-repr-null? u))
                     (or (not (type-repr-false? t)) (type-repr-false? u))
                     (or (not (type-repr-true? t)) (type-repr-true? u)))))))
(define (type-intersection2 t u)
  (cond ((or (eq? type-bottom t) (eq? type-bottom u)) type-bottom)
        ((eq? type-top t) u)
        ((eq? type-top u) t)
        (else (let ((proc-t (type-repr-procedure t))
                    (proc-u (type-repr-procedure u))
                    (pair-t (type-repr-pair t))
                    (pair-u (type-repr-pair u)))
                (type-repr
                  (and proc-t proc-u)  ;; TODO: procedure details.
                  (and pair-t pair-u
                       `(,(type-intersection2 (car pair-t) (car pair-u))
                          . ,(type-intersection2 (cdr pair-t) (cdr pair-u))))
                  (and (type-repr-number? t) (type-repr-number? u))
                  (and (type-repr-symbol? t) (type-repr-symbol? u))
                  (and (type-repr-null? t) (type-repr-null? u))
                  (and (type-repr-false? t) (type-repr-false? u))
                  (and (type-repr-true? t) (type-repr-true? u)))))))
(define (type-union2 t u)
  (cond ((or (eq? type-top t) (eq? type-top u)) type-top)
        ((eq? type-bottom t) u)
        ((eq? type-bottom u) t)
        (else (let ((proc-t (type-repr-procedure t))
                    (proc-u (type-repr-procedure u))
                    (pair-t (type-repr-pair t))
                    (pair-u (type-repr-pair u)))
                (type-repr
                  (or proc-t proc-u)  ;; TODO: procedure details.
                  (cond ((not pair-t) pair-u)
                        ((not pair-u) pair-t)
                        (else `(,(type-union2 (car pair-t) (car pair-u))
                                 . ,(type-union2 (cdr pair-t) (cdr pair-u)))))
                  (or (type-repr-number? t) (type-repr-number? u))
                  (or (type-repr-symbol? t) (type-repr-symbol? u))
                  (or (type-repr-null? t) (type-repr-null? u))
                  (or (type-repr-false? t) (type-repr-false? u))
                  (or (type-repr-true? t) (type-repr-true? u)))))))

(define (type-intersection ts) (foldl type-intersection2 type-top ts))
(define (type-union ts) (foldl type-union2 type-bottom ts))

;; TODO: these definitions probably belong somewhere else.
(define procedure-tag (gensym "#%procedure"))
(define (closure-new lam cenv) `(,procedure-tag ,lam ,cenv))

(define (datum->type datum)
  (cond ((var? datum) type-top)
        ((and (pair? datum) (eq? procedure-tag (car datum)))
         (type-procedure #f type-top))  ;; TODO: retrieve procedure details.
        ((pair? datum) (type-pair (datum->type (car datum))
                                  (datum->type (cdr datum))))
        ((number? datum) type-number)
        ((symbol? datum) type-symbol)
        ((null? datum) type-null)
        ((not datum) type-false)
        ((eq? #t datum) type-true)
        (else (error 'datum->type-tag (format "unexpected datum: ~s" datum)))))

(define (zip-with f xss) (apply map f xss))
(define (zip xss) (zip-with list xss))

(define (env-flatten env-raw)
  (let loop ((env env-raw) (seen '()))
    (if (null? env) '()
      (let ((rib (car env)))
        (if (eq? 'val (car rib))
          (if (member (cadr rib) seen) (loop (cdr env) seen)
            (cons (cdr rib) (loop (cdr env) (cons (cadr rib) seen))))
          (let rec-loop ((rec (cdr rib)) (seen seen))
            (if (null? rec) (loop (cdr env) seen)
              (if (member (caar rec) seen) (rec-loop (cdr rec) seen)
                (cons (cons (caar rec) (closure-new (cdar rec) env))
                      (rec-loop (cdr rec) (cons (caar rec) seen)))))))))))

(define (env->type env)
  (map (lambda (b) (cons (car b) (datum->type (cdr b)))) env))
;; TODO: ideally we wouldn't repeat work for shared env tails.
(define (env*->type env*)
  (define et* (map env->type env*))
  (define name* (map car (car et*)))
  (define type** (map (lambda (t*) (map cdr t*)) et*))
  (define type* (map type-union (zip type**)))
  (zip-with cons (list name* type*)))


;; TODO:
;; Flag whether any env identifiers are logic variables.
;;   If so, may need to perform a =/= pass to guarantee unshadowed primitives.
;; Determine which primitive syntactic forms and operators are available.

;; If none of the analysis solutions to a parallel evalo problem suffice, it's probably because there's an unusual constraint.  Move this problem to a failure queue, to retry with normal search after all remaining analysis problems are attempted.

;; TODO: parallel env analysis
;;   useful procedure applications (extension of basic data units)
;;   useful (partitioning) conditions
;;     optional: simple rules for "pair + one other type" conditions
;;       e.g., "pair + null" may mean we prefer checking "null?" over "pair?"

(define (basic-data* env* result*)
  ;; Memoize/cache basic-data of a given result type, for type-based lookup.


  ;; Subdivide these by type in some cases?
  ;;   predicates only useful when result* type is exactly: (union #t #f)
  ;;   cons only useful when result* type is exactly: (single pair)

  ;; number n ::= ...
  ;; symbol s ::= ...
  ;; literal l ::= (l . l) | n | s | () | #f | #t
  ;; extracted e ::= variable | (car e) | (cdr e)
  ;; predicate ::= (equal? e e) | (procedure? e) | (pair? e) | (number? e)
  ;;             | (symbol? e) | (null? e) | (not e)
  ;;             | (equal? #t e) | (equal? n e) | (equal? s e)
  ;; built ::= e | predicate
  ;;         | (cons built built) | (cons built literal) | (cons literal built)
  ;; all ::= built | literal
  ;;
  ;; NOTE: predicates are only interesting if their results differ across
  ;; environments (some produce #t, some #f).

  #f
  )


;; TODO: expression typing for conditional branch lookahead

;; TODO: primitive result types:
;; pair -> top: car cdr
;; top -> top -> pair: cons
;; [-> top] top -> boolean: not equal? symbol? number? null? pair? procedure?
;;
;; missing primitives: apply: top -> pair -> top
