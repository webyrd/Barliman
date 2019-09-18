;; starting point is miniKanren tabling code by Ramana Kumar
;; taken from webyrd/tabling

(define ext-s-no-check
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define new-s
  (lambda (c s)
    (cons s (cdr c))))

(define make-cache (lambda (ansv*) (vector 'cache ansv*)))
(define cache-ansv* (lambda (v) (vector-ref v 1)))
(define cache-ansv*-set! (lambda (v val) (vector-set! v 1 val)))

(define make-ss (lambda (cache ansv* f) (vector 'ss cache ansv* f)))
(define ss? (lambda (v) (and (vector? v) (eq? (vector-ref v 0) 'ss))))
(define ss-cache (lambda (v) (vector-ref v 1)))
(define ss-ansv* (lambda (v) (vector-ref v 2)))
(define ss-f (lambda (v) (vector-ref v 3)))

(define subunify (lambda (arg ans s) (subsumed ans arg s)))

(define subsumed
  (lambda (arg ans s)
    (let ((arg (walk arg s))
          (ans (walk ans s)))
      (cond
        ((eq? arg ans) s)
        ((var? ans) (ext-s-no-check ans arg s))
        ((and (pair? arg) (pair? ans))
         (let ((s (subsumed (car arg) (car ans) s)))
           (and s (subsumed (cdr arg) (cdr ans) s))))
        ((equal? arg ans) s)
        (else #f)))))

(define reuse
  (lambda (argv cache s c)
    (let fix ((start (cache-ansv* cache)) (end '()))
      (let loop ((ansv* start))
        (if (eq? ansv* end)
            (list (make-ss cache start (lambdaf@ () (fix (cache-ansv* cache) start))))
            (choice (new-s c (subunify argv (reify-var (car ansv*) s) s))
                    (lambdaf@ () (loop (cdr ansv*)))))))))

(define master
  (lambda (argv cache)
    (lambdag@ (c : S D A T M)
      (bind*
       (purge-M-inc-models c)
       (lambdag@ (c : S D A T M)
         (and
          (for-all
           (lambda (ansv) (not (subsumed argv ansv S)))
           (cache-ansv* cache))
          (begin
            (cache-ansv*-set! cache
                              (cons (reify-var argv S)
                                    (cache-ansv* cache)))
            c)))))))

(define-syntax tabled
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (let ((table '()))
       (lambda (x ...)
         (let ((argv (list x ...)))
           (lambdag@ (c : S D A T M)
             (let ((key ((reify argv) c)))
               (cond
                 ((assoc key table)
                  => (lambda (key.cache) (reuse argv (cdr key.cache) S c)))
                 (else (let ((cache (make-cache '())))
                         (set! table (cons `(,key . ,cache) table))
                         ((fresh () g g* ... (master argv cache)) c))))))))))))


(define ss-ready? (lambda (ss) (not (eq? (cache-ansv* (ss-cache ss)) (ss-ansv* ss)))))
(define w? (lambda (w) (and (pair? w) (ss? (car w)))))

(define w-check
  (lambda (w sk fk)
    (let loop ((w w) (a '()))
      (cond
        ((null? w) (fk))
        ((ss-ready? (car w))
         (sk (lambdaf@ ()
               (let ((f (ss-f (car w)))
                     (w (append (reverse a) (cdr w))))
                 (if (null? w) (f)
                     (mplus (f) (lambdaf@ () w)))))))
        (else (loop (cdr w) (cons (car w) a)))))))

(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((w) ew) ((c^) e2) ((c f) e3))
     (let ((c-inf e))
       (cond
         ((not c-inf) e0)
         ((procedure? c-inf)  (let ((f^ c-inf)) e1))
         ((w? c-inf) (w-check c-inf
                              (lambda (f^) e1)
                              (lambda () (let ((w c-inf)) ew))))
         ((not (and (pair? c-inf)
                 (procedure? (cdr c-inf))))
          (let ((c^ c-inf)) e2))
         (else (let ((c (car c-inf)) (f (cdr c-inf)))
                 e3)))))))

(define take
  (lambda (n f)
    (cond
      ((and n (zero? n)) '())
      (else
       (case-inf (f)
         (() '())
         ((f) (take n f))
         ((w) '())
         ((c) (cons c '()))
         ((c f) (cons c (take (and n (- n 1)) f))))))))

(define bind
  (lambda (c-inf g)
    (case-inf c-inf
      (() (mzero))
      ((f) (inc (bind (f) g)))
      ((w) (map (lambda (ss)
                  (make-ss (ss-cache ss) (ss-ansv* ss)
                           (lambdaf@ () (bind ((ss-f ss)) g))))
                w))
      ((c) (g c))
      ((c f) (mplus (g c) (lambdaf@ () (bind (f) g)))))))

(define mplus
  (lambda (c-inf f)
    (case-inf c-inf
      (() (f))
      ((f^) (inc (mplus (f) f^)))
      ((w) (lambdaf@ () (let ((c-inf (f)))
                          (if (w? c-inf)
                              (append c-inf w)
                              (mplus c-inf (lambdaf@ () w))))))      ((c) (choice c f))
      ((c f^) (choice c (lambdaf@ () (mplus (f) f^)))))))

(define reify-v
  (lambda (n)
    (var n)))

(define empty-S '())

(define make-reify
  (lambda (rep)
    (lambda (v s)
      (let ((v (walk* v s)))
        (walk* v (reify-s rep v empty-S))))))

(define reify-s
  (lambda (rep v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (ext-s-no-check v (rep (length s)) s))
        ((pair? v) (reify-s rep (cdr v) (reify-s rep (car v) s)))
        (else s)))))

(define reify-from-s (make-reify reify-name))
(define reify-var (make-reify reify-v))
