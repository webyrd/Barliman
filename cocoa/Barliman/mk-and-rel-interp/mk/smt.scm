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

(define undeclared?
  (lambda (ds) (if (null? ds) (lambda (x) #t) (lambda (x) (not (memq x ds))))))

(define (range a b)
  (if (>= a b) '() (cons a (range (1+ a) b))))

(define map-to-list
  (lambda (S) S))

(define z/reify-SM
  (lambda (M)
    (lambda (st)
      (let* ((S (state-S st))
             (M (walk* (reverse M) S))
             (S (reify-S M (subst empty-subst-map nonlocal-scope)))
             (M (walk* M S))
             (dd-M (partition declare-datatypes? M))
             (dd (car dd-M))
             (M (cdr dd-M))
             (ds-R (partition declares? M))
             (ds (car ds-R))
             (R (cdr ds-R))
             (ds (filter-redundant-declares ds ds))
             (M (append ds R))
             (S (map-to-list (subst-map S))))
        (cons
         (map (lambda (x) (cons (cdr x) (car x))) S)
         (append
          dd
          (map (lambda (x) `(declare-fun ,x () Int))
               (filter (undeclared? (map cadr ds)) (map cdr S)))
          M))))))

(define defer-smt-checks #f)

(define z/check
  (lambdag@ (st)
    (if (or defer-smt-checks (check-sat (cdr ((z/reify-SM (state-M st)) st))))
        st
        #f)))

(define z/
  (lambda (line)
    (lambdag@ (st)
      (let ((M (cons line (state-M st))))
        (state (state-S st) (state-C st) (state-depth st) (state-deferred st) M)))))

(define z/assert
  (lambda (e)
    (fresh ()
      (z/ `(assert ,e))
      ;;z/check
      )))

(define add-model
  (lambda (m s)
    (lambda (st)
      (if (null? m)
          st
          (bind
           ((== (cdr (assq (caar m) s)) (cdar m)) st)
           (add-model (cdr m) s))))))

(define get-next-model? #t)

(define z/purge
  (lambdag@ (st)
    (let ((M (state-M st)))
      (if (null? M)
          st
          (let ([SM ((z/reify-SM M) st)])
            (if (not (check-sat (cdr SM)))
                #f
                (if (null? (car SM))
                    (state (state-S st) (state-C st) (state-depth st) (state-deferred st) '())
                    (let loop ((ms '()))
                      (let ((m (get-next-model (cdr SM) ms)))
                        (and m
                             (let ((st (state-with-scope st (new-scope))))
                               (mplus
                                ((add-model m (car SM))
                                 (state (state-S st) (state-C st) (state-depth st) (state-deferred st) '()))
                                (lambda () (if get-next-model?
                                          (loop (cons m ms))
                                          #f))))))))))))))
