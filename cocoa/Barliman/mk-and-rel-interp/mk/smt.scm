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
               (M (append ds R))
               (_ (set! decls (append (map cadr ds) decls)))
               (dc (map (lambda (x) `(declare-fun ,x () Int))
                        (filter undeclared? (map reify-v-name vs)))))
          (list
           'legacy
           (append
            dd
            dc
            M)
           vs))))))

(define (get-assumptions a)
  (let ((pos (assq a assumption-chains)))
    (map (lambda (b)
           (if (memq b pos)
               b
               `(not ,b)))
         (reverse all-assumptions))))
(define (check-sat-assuming a xs)
  (call-z3 `(,@xs (check-sat-assuming ,(get-assumptions a))))
  (read-sat))

(define (take-until p xs)
  (if (null? xs)
      '()
      (if (p (car xs))
          (cons (car xs) '())
          (cons (car xs) (take-until p (cdr xs))))))

(define z/varo
  (lambda (u)
    (lambdag@ (st)
      (let ((term (walk u (state-S st))))
        (if (var? term)
            (let* ((c (lookup-c term st))
                   (M (c-M c)))
              (if M st
                  (set-c term (c-with-M c #t) st)))
            st)))))


(define (z/check a no_walk?)
  (lambdag@ (st)
    (let ((r ((z/reify-SM (take-until (lambda (x) (equal? x `(declare-const ,a Bool)))
                                      (state-M st))
                          no_walk?) st)))
      (if (check-sat-assuming a (cadr r))
          (begin
            (let ((p (assq a relevant-vars)))
              (set-cdr! p (append (caddr r) (cdr p))))
            ((let loop ((vs (caddr r)))
               (lambdag@ (st)
                 (if (null? vs)
                     st
                     (bind*
                      st
                      (z/varo (car vs))
                      (loop (cdr vs))))))
             st))
          #f))))

(define z/
  (lambda (line)
    (lambdag@ (st)
      (let ((M (cons line (state-M st))))
        (state-with-M st M)))))

(define assumption-count 0)
(define (fresh-assumption)
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
            (let ((rs (if (eq? a0 'true) '()  (cdr (assq a0 relevant-vars))))
                  (as (if (eq? a0 'true) '() (assq a0 assumption-chains))))
              (set! relevant-vars (cons (cons a1 rs) relevant-vars))
              (set! assumption-chains (cons (cons a1 as) assumption-chains))
              (set! all-assumptions (cons a1 all-assumptions))
              (bind*
               st
               (z/ `(declare-const ,a1 Bool))
               (z/ `(assert (=> ,a1 ,e)))
               (z/check a1 no_walk?)))))))))

(define relevant-vars '())
(define assumption-chains '())
(define all-assumptions '())
(define (z/reset!)
  (call-z3 '((reset)))
  (set! decls '())
  (set! relevant-vars '())
  (set! assumption-chains '())
  (set! all-assumptions '())
  (set! assumption-count 0)
  (set! m-subst-map empty-subst-map))

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
                (if (not (check-sat-assuming a '()))
                    #f
                    (let ([rs (map (lambda (x) (cons (reify-v-name x) x)) (cdr (assq a relevant-vars)))])
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
