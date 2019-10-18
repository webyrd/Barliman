(define empty-ap-map empty-subst-map)
(define assumption->int
  (lambda (a)
    (let ((s (symbol->string a)))
      (string->number (substring s 2 (string-length s))))))
(define ap-map-lookup
  (lambda (u S)
    (data-val (t:lookup (assumption->int u) S))))
(define (ap-map-add S var val)
  (t:bind (assumption->int var) val S))

(define partition
  (lambda (p xs)
    (cons (filter p xs)
          (filter (lambda (x) (not (p x))) xs))))

(define declare-datatypes?
  (lambda (s)
    (and (pair? s)
         (or (eq? 'declare-datatypes (car s))
             (eq? 'declare-sort (car s)))
         (cadr s))))

(define declares?
  (lambda (s)
    (and (pair? s)
         (or (eq? 'declare-fun (car s))
             (eq? 'declare-const (car s)))
         (cadr s))))

(define filter-redundant-declare
  (lambda (d es)
    (filter
     (lambda (e)
       (or (not (eq? (cadr e) (cadr d)))
           (if (equal? e d) #f
               (error 'filter-redundant-declare "inconsistent" d e))))
     es)))

(define filter-redundant-declares
  (lambda (ds es)
    (if (null? ds)
        es
        (filter-redundant-declares
         (cdr ds)
         (cons (car ds) (filter-redundant-declare (car ds) es))))))

(define decls '())
(define undeclared?
  (lambda (x)
    (let ((r (not (memq x decls))))
      (when r
        (set! decls (cons x decls)))
      r)))

(define (range a b)
  (if (>= a b) '() (cons a (range (1+ a) b))))

(define m-subst-map empty-subst-map)
(define (reify-v-name v)
  (string->symbol (format #f "_v~d" (var-idx v))))

(define reify-M
  (lambda (vs S)
    (if (null? vs)
        S
        (let ((S (reify-M (cdr vs) S))
              (v (car vs)))
          (if (eq? unbound (subst-map-lookup v S))
              (subst-map-add S v (reify-v-name v))
              S)))))

(define walkr
  (lambda (u S)
    (if (var? u)
      (let ((val (subst-map-lookup u S)))
        (if (eq? val unbound)
          u
          (walkr val S)))
      u)))
(define walkr*
  (lambda (v S)
    (let ((v (walkr v S)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons (walkr* (car v) S) (walkr* (cdr v) S)))
        (else v)))))

(define z/reify-SM
  (lambda (M . args)
    (let ((no_walk? (and (not (null? args)) (car args))))
      (lambda (st)
        (let* ((S (state-S st))
               (M (reverse M))
               (M (if no_walk? M (walk* M S)))
               (vs (vars M '()))
               (S (reify-M vs m-subst-map))
               (_ (set! m-subst-map S))
               (M (walkr* M S))
               (dd-M (partition declare-datatypes? M))
               (dd (car dd-M))
               (M (cdr dd-M))
               (ds-R (partition declares? M))
               (ds (car ds-R))
               (R (cdr ds-R))
               (ds (filter-redundant-declares ds ds))
               (_ (set! decls (append (map cadr ds) decls)))
               (dc (map (lambda (x) `(declare-const ,x Int))
                        (filter undeclared? (map reify-v-name vs)))))
          (list
           dd
           (append
            ds
            dc
            R)
           vs))))))

(define (get-assumptions a)
  (let ((pos (ap-map-lookup a assumption-chains)))
    (map (lambda (b)
           (if (memq b pos)
               b
               `(not ,b)))
         (reverse all-assumptions))))
(define (check-sat-assuming a m)
  (replay-if-needed a m)
  (call-z3 `((check-sat-assuming ,(get-assumptions a))))
  (read-sat))

(define (take-until p xs)
  (if (null? xs)
      '()
      (if (p (car xs))
          '()
          (cons (car xs) (take-until p (cdr xs))))))

(define (take-while p xs)
  (if (null? xs)
      '()
      (if (p (car xs))
          (cons (car xs) (take-while p (cdr xs)))
          '())))

(define (smt-ok? st x)
  (let ((x (walk* x (state-S st))))
    (or (number? x)
        (and (var? x)
             (let ((c (lookup-c x st)))
               (c-M c))))))

(define (filter-smt-ok? st D)
  (filter
   (lambda (cs)
     (for-all (lambda (ds)
                (and (smt-ok? st (car ds)) (smt-ok? st (cdr ds))))
              cs))
   D))

(define (add-smt-disequality st D)
  (let ((as (filter-smt-ok? st D)))
    (if (not (null? as))
        (z/assert
         `(and
           ,@(map
              (lambda (cs)
                `(or
                  ,@(map
                     (lambda (ds)
                       `(not (= ,(car ds) ,(cdr ds))))
                     cs)))
              as))
         #t)
        (lambdag@ (st) st))))

(define z/varo
  (lambda (u)
    (lambdag@ (st)
      (let ((term (walk u (state-S st))))
        (if (var? term)
            (let* ((c (lookup-c term st))
                   (M (c-M c))
                   (D (c-D c)))
              (bind*
               st
               (lambdag@ (st)
                 (if M st
                     (set-c term (c-with-M c #t) st)))
               (if (or M (null? D))
                   (lambdag@ (st) st)
                   (lambdag@ (st) ((add-smt-disequality st D) st)))))
            st)))))

(define global-buffer '())
(define z/global
  (lambda (lines)
    (call-z3 lines)
    (set! global-buffer (append global-buffer lines))))
(define local-buffer '())
(define z/local
  (lambda (lines)
    (lambdag@ (st)
      (begin
        (set! local-buffer (append local-buffer lines))
        (call-z3 lines)
        (let ((M (append (reverse lines) (state-M st))))
          (state-with-M st M))))))
(define (replay-if-needed a m)
  (let ((r (filter (lambda (x) (not (member x local-buffer))) m)))
    (unless (null? r)
      (let ((lines (reverse r)))
        (let ((new-decls  (filter (lambda (x)
                                    (and (declares? x)
                                         (not (eq? (caddr x) 'Bool))))
                                  lines))
              (new-assumptions (filter (lambda (x)
                                         (and (declares? x)
                                              (eq? (caddr x) 'Bool)))
                                       lines))
              (other-lines (filter (lambda (x) (not (declares? x))) lines)))
          (let* ((undeclared-decls (filter (lambda (x) (undeclared? (cadr x))) new-decls))
                 (undeclared-assumptions (filter (lambda (x) (undeclared? (cadr x))) new-assumptions))
                 (actual-lines (append undeclared-decls undeclared-assumptions other-lines)))
            (let* ((rs (filter undeclared? (map reify-v-name (ap-map-lookup a relevant-vars))))
                   (undeclared-rs (map (lambda (x) `(declare-const ,x Int)) rs))
                   (actual-lines (append undeclared-rs actual-lines)))
              (set! all-assumptions (append (map cadr undeclared-assumptions) all-assumptions))
              (set! local-buffer (append local-buffer actual-lines))
              (call-z3 actual-lines))))))))

(define (z/check m a no_walk?)
  (lambdag@ (st)
    (begin
      (replay-if-needed (last-assumption (state-M st)) (state-M st))
      (let ((r ((z/reify-SM m no_walk?) st)))
        (z/global (car r))
        (bind*
         st
         (z/local (cadr r))
         (lambdag@ (st)
           (if (and a (check-sat-assuming a (state-M st)))
               (begin
                 (let ((rs (ap-map-lookup a relevant-vars)))
                   (set! relevant-vars (ap-map-add relevant-vars a (append (caddr r) rs))))
                 ((let loop ((vs (caddr r)))
                    (lambdag@ (st)
                      (if (null? vs)
                          st
                          (bind*
                           st
                           (numbero (car vs))
                           (z/varo (car vs))
                           (loop (cdr vs))))))
                  st))
               (if a #f st))))))))

(define (z/ line)
  (z/check (list line) #f #t))

(define assumption-count 0)
(define (fresh-assumption)
  (when (and (= (remainder assumption-count 100) 0)
             (> assumption-count 0))
    ;;(printf "gc z3...\n")
    (z/gc!))
  (set! assumption-count (+ assumption-count 1))
  (string->symbol (format #f "_a~d" assumption-count)))

(define (last-assumption m)
  (let ((r (filter (lambda (x) (and (pair? x)
                               (eq? 'assert (car x))
                               (pair? (cadr x))
                               (eq? (car (cadr x)) '=>)))
                   m)))
    (if (null? r)
        'true
        (cadr (cadr (car r))))))

(define z/assert
  (lambda (e . args)
    (let ((no_walk? (and (not (null? args)) (car args))))
      (lambdag@ (st)
        (let ((a1 (fresh-assumption)))
          (let ((a0 (last-assumption (state-M st))))
            (let ((rs (if (eq? a0 'true) '()  (ap-map-lookup a0 relevant-vars)))
                  (as (if (eq? a0 'true) '() (ap-map-lookup a0 assumption-chains))))
              (set! relevant-vars (ap-map-add relevant-vars a1 rs))
              (set! assumption-chains (ap-map-add assumption-chains a1 (cons a1 as)))
              (set! all-assumptions (cons a1 all-assumptions))
              (bind*
               st
               (z/check `((assert (=> ,a1 ,e))
                          (declare-const ,a1 Bool))
                        a1
                        no_walk?)))))))))

(define relevant-vars empty-ap-map)
(define assumption-chains empty-ap-map)
(define all-assumptions '())
(define (z/reset!)
  (call-z3 '((reset)))
  (set! decls '())
  (set! relevant-vars empty-ap-map)
  (set! assumption-chains empty-ap-map)
  (set! all-assumptions '())
  (set! assumption-count 0)
  (set! m-subst-map empty-subst-map)
  (set! global-buffer '())
  (set! local-buffer '()))
(define (z/gc!)
  (call-z3 '((reset)))
  (call-z3 global-buffer)
  (set! decls '())
  (set! all-assumptions '())
  (set! local-buffer '()))

(define add-model
  (lambda (m)
    (lambdag@ (st)
      (if (null? m)
          st
          (bind*
           st
           (== (caar m) (cdar m))
           (add-model (cdr m)))))))

(define assert-neg-model
  (lambda (m)
    (let* ([m
            (filter (lambda (x) ; ignoring functions
                      (or (number? (cdr x))
                          (symbol? (cdr x)) ; for bitvectors
                          )) m)])
      (if (null? m)
          fail
          (z/assert (cadr (neg-model m)))))))

(define z/purge
  (lambdag@ (st)
    (let ((M (state-M st)))
      (if (null? M)
          st
          (let ([a (last-assumption (state-M st))])
            (if (eq? a 'true)
                st
                (if (not (check-sat-assuming a (state-M st)))
                    #f
                    (let ([rs (map (lambda (x) (cons (reify-v-name x) x)) (ap-map-lookup a relevant-vars))])
                      ((let loop ()
                         (lambdag@ (st)
                           (let ((m (get-model-inc)))
                             (let ((m (map (lambda (x) (cons (cdr (assq (car x) rs)) (cdr x))) (filter (lambda (x) (assq (car x) rs)) m))))
                               (let ((st (state-with-scope st (new-scope))))
                                 (mplus*
                                  (bind*
                                   (state-with-M st '())
                                   (add-model m))
                                  (bind*
                                   st
                                   (assert-neg-model m)
                                   (loop))))))))
                       st)))))))))
