; Scope object.
; Used to determine whether a branch has occured between variable creation
; and unification to allow the set-var-val! optimization in subst-add. Both variables
; and substitutions will contain a scope. When a substitution flows through a
; conde it is assigned a new scope.

;; To allow use of optimizations that sacrifice completeness, set this to #t.
(define allow-incomplete-search? #f)

;; To allow use of experimental `conde1` optimization, set this to #t.
(define enable-conde1? #t)

; Creates a new scope that is not scope-eq? to any other scope
(define new-scope
  (lambda ()
    (list 'scope)))

; Scope used when variable bindings should always be made in the substitution,
; as in disequality solving and reification. We don't want to set-var-val! a
; variable when checking if a disequality constraint holds!
(define nonlocal-scope
  (list 'non-local-scope))

(define scope-eq? eq?)


; Logic variable object.
; Contains:
;   val - value for variable assigned by unification using set-var-val! optimization.
;           unbound if not yet set or stored in substitution.
;   scope - scope that the variable was created in.
;   idx - unique numeric index for the variable. Used by the trie substitution representation.
; Variable objects are compared by object identity.

; The unique val for variables that have not yet been bound to a value
; or are bound in the substitution
(define unbound (list 'unbound))

(define var
  (let ((counter -1))
    (lambda (scope)
      (set! counter (+ 1 counter))
      (vector unbound scope counter))))

; Vectors are not allowed as terms, so terms that are vectors are variables.
(define var?
  (lambda (x)
    (vector? x)))

(define var-eq? eq?)

(define var-val
  (lambda (x)
    (vector-ref x 0)))

(define set-var-val!
  (lambda (x v)
    (vector-set! x 0 v)))

(define var-scope
  (lambda (x)
    (vector-ref x 1)))

(define var-idx
  (lambda (x)
    (vector-ref x 2)))


; Substitution object.
; Contains:
;   map - mapping of variables to values
;   scope - scope at current program point, for set-var-val! optimization. Updated at conde.
;               Included in the substitution because it is required to fully define the substitution
;               and how it is to be extended.
;
; Implementation of the substitution map depends on the Scheme used, as we need a map. See mk.rkt
; and mk-vicare.scm.

(define subst
  (lambda (mapping scope)
    (cons mapping scope)))

(define subst-map car)

(define subst-scope cdr)

(define subst-length
  (lambda (S)
    (subst-map-length (subst-map S))))

(define subst-with-scope
  (lambda (S new-scope)
    (subst (subst-map S) new-scope)))

(define empty-subst (subst empty-subst-map (new-scope)))

(define subst-add
  (lambda (S x v)
    ; set-var-val! optimization: set the value directly on the variable
    ; object if we haven't branched since its creation
    ; (the scope of the variable and the substitution are the same).
    ; Otherwise extend the substitution mapping.
    (if (scope-eq? (var-scope x) (subst-scope S))
      (begin
        (set-var-val! x v)
        S)
      (subst (subst-map-add (subst-map S) x v) (subst-scope S)))))

(define subst-lookup
  (lambda (u S)
    ; set-var-val! optimization.
    ; Tried checking the scope here to avoid a subst-map-lookup
    ; if it was definitely unbound, but that was slower.
    (if (not (eq? (var-val u) unbound))
      (var-val u)
      (subst-map-lookup u (subst-map S)))))

; Association object.
; Describes an association mapping the lhs to the rhs. Returned by unification
; to describe the associations that were added to the substitution (whose representation
; is opaque) and used to represent disequality constraints.

(define lhs car)

(define rhs cdr)

; Constraint record object.
;
; Describes the constraints attached to a single variable.
;
; Contains:
;   T - type constraint. 'symbolo 'numbero or #f to indicate no constraint
;   D - list of disequality constraints. Each disequality is a list of associations.
;           The constraint is violated if all associated variables are equal in the
;           substitution simultaneously. D could contain duplicate constraints (created
;           by distinct =/= calls). A given disequality constraint is only attached to
;           one of the variables involved, as all components of the constraint must be
;           violated to cause failure.
;   A - list of absento constraints. Each constraint is a ground atom. The list contains
;           no duplicates.

(define empty-c `(#f () ()))

(define c-T
  (lambda (c)
    (car c)))

(define c-D
  (lambda (c)
    (cadr c)))

(define c-A
  (lambda (c)
    (caddr c)))

(define c-with-T
  (lambda (c T)
    (list T (c-D c) (c-A c))))

(define c-with-D
  (lambda (c D)
    (list (c-T c) D (c-A c))))

(define c-with-A
  (lambda (c A)
    (list (c-T c) (c-D c) A)))

; Constraint store object.
; Mapping of representative variable to constraint record. Constraints are
; always on the representative element and must be moved / merged when that
; element changes.

; Implementation depends on the Scheme used, as we need a map. See mk.rkt
; and mk-vicare.scm.

; State object.
; The state is the value that is monadically passed through the search.
; Contains:
;   S - the substitution
;   C - the constraint store
;   depth - the current search depth in terms of `conde` nesting
;   deferred - expensive goals that will be tried later

; TODO: use set! to choose appropriate max-search-depth per-run?
(define max-search-depth
  ;#f  ; unlimited depth
  ; Is this a reasonable depth for typical Barliman problems?
  100)

(define state
  (lambda (S C depth deferred)
    (list S C depth deferred)))

(define state-S (lambda (st) (car st)))
(define state-C (lambda (st) (cadr st)))
(define state-depth (lambda (st) (caddr st)))
(define state-deferred (lambda (st) (cadddr st)))
(define state-depth-set
  (lambda (st depth)
    (state (state-S st) (state-C st) depth (state-deferred st))))
(define state-depth-deepen
  (lambda (st)
    (let ((next-depth (+ 1 (state-depth st))))
      (if (and allow-incomplete-search?
               max-search-depth (< max-search-depth next-depth))
        (mzero)
        (state (state-S st) (state-C st) next-depth (state-deferred st))))))
(define state-deferred-defer
  (lambda (st goal)
    (let ((deferred (state-deferred st)))
      (if deferred
        (state (state-S st)
               (state-C st)
               (state-depth st)
               (cons goal (state-deferred st)))
        (goal st)))))
(define state-deferred-defer*
  (lambda (st goals)
    (let ((deferred (state-deferred st)))
      (if deferred
        (state (state-S st)
               (state-C st)
               (state-depth st)
               (append goals (state-deferred st)))
        ((resume goals) st)))))
(define (resume goals)
  (if (null? goals)
    unit
    (fold-left (lambda (g0 g1)
                 (lambda (st)
                   (bind (g0 st) g1)))
               (car goals) (cdr goals))))
(define state-deferred-resume
  (lambda (st)
    (let ((deferred (state-deferred st)))
      (if (and deferred (pair? deferred))
        ;; TODO: fix this once conde1 is obsolete.
        ((fold-left (lambda (g1 g0)
                      (lambda (st)
                        (bind (g0 st) g1)))
                    unit
                    deferred)
         (state (state-S st) (state-C st) (state-depth st) #f))
        st))))
(define (state-deferred-set st deferred)
  (state (state-S st) (state-C st) (state-depth st) deferred))
(define (state-deferred-clear st) (state-deferred-set st '()))

(define-syntax let-deferred
  (syntax-rules ()
    ((_ (deferred g0 gs ...) body ...)
     (lambda (st)
       (let ((saved (state-deferred st)))
         (bind*-depth
           (state-deferred-clear st) g0 gs ...
           (lambda (st)
             (let ((deferred (state-deferred st)))
               (bind*-depth (state-deferred-set st saved) body ...)))))))))
(define (defer goal) (lambda (st) (state-deferred-defer st goal)))
(define (defer* gs) (lambda (st) (state-deferred-defer* st gs)))

(define (empty-state) (state empty-subst empty-C 0 (and enable-conde1? '())))

(define state-with-scope
  (lambda (st new-scope)
    (state (subst-with-scope (state-S st) new-scope)
           (state-C st)
           (state-depth st)
           (state-deferred st))))

; Unification

(define walk
  (lambda (u S)
    (if (var? u)
      (let ((val (subst-lookup u S)))
        (if (eq? val unbound)
          u
          (walk val S)))
      u)))

(define occurs-check
  (lambda (x v S)
    (let ((v (walk v S)))
      (cond
        ((var? v) (var-eq? v x))
        ((pair? v)
         (or
           (occurs-check x (car v) S)
           (occurs-check x (cdr v) S)))
        (else #f)))))

(define ext-s-check
  (lambda (x v S)
    (cond
      ((occurs-check x v S) (values #f #f))
      (else (values (subst-add S x v) `((,x . ,v)))))))

; Returns as values the extended substitution and a list of associations added
; during the unification, or (values #f #f) if the unification failed.
;
; Right now appends the list of added values from sub-unifications. Alternatively
; could be threaded monadically, which could be faster or slower.
(define unify
  (lambda (u v s)
    (let ((u (walk u s))
          (v (walk v s)))
      (cond
        ((eq? u v) (values s '()))
        ((var? u) (ext-s-check u v s))
        ((var? v) (ext-s-check v u s))
        ((and (pair? u) (pair? v))
         (let-values (((s added-car) (unify (car u) (car v) s)))
           (if s
             (let-values (((s added-cdr) (unify (cdr u) (cdr v) s)))
               (values s (append added-car added-cdr)))
             (values #f #f))))
        ((equal? u v) (values s '()))
        (else (values #f #f))))))

(define unify*
  (lambda (S+ S)
    (unify (map lhs S+) (map rhs S+) S)))


; Search

; Search result types. Names inspired by the plus monad?
(define mzero (lambda () #f))
(define unit (lambda (c) c))
(define choice (lambda (c f) (cons c f)))

(define-syntax inc
  (syntax-rules ()
    ((_ e) (lambda () e))))

(define empty-f (inc (mzero)))
(define pause (lambda (c) (inc c)))

(define-syntax lambdag@
  (syntax-rules ()
    ((_ (st) e) (lambda (st) e))))

(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((c^) e2) ((c f) e3))
     (let ((c-inf e))
       (cond
         ((not c-inf) e0)
         ((procedure? c-inf)  (let ((f^ c-inf)) e1))
         ((not (and (pair? c-inf)
                 (procedure? (cdr c-inf))))
          (let ((c^ c-inf)) e2))
         (else (let ((c (car c-inf)) (f (cdr c-inf)))
                 e3)))))))

(define-syntax let/vars
  (syntax-rules ()
    ((_ _ () body) body)
    ((_ _ () body ...) (begin body ...))
    ((_ st (qvar ...) body ...)
     (let ((scope (subst-scope (state-S st))))
       (let ((qvar (var scope)) ...)
         body ...)))))

(define-syntax with-depth
  (syntax-rules ()
    ((_ dst g) (lambdag@ (st) (g (state-depth-set st (state-depth dst)))))))

(define-syntax bind*-depth
  (syntax-rules ()
    ((_ st0 g0 g ...) (bind* (g0 st0) (with-depth st0 g) ...))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (st)
       (inc
         (let ((scope (subst-scope (state-S st))))
           (let ((x (var scope)) ...)
             (bind*-depth st g0 g ...))))))))

(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))

(define bind
  (lambda (c-inf g)
    (case-inf c-inf
      (() (mzero))
      ((f) (inc (bind (f) g)))
      ((c) (g c))
      ((c f) (mplus (g c) (inc (bind (f) g)))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (take n
       (inc
         ((fresh (q) g0 g ... state-deferred-resume
            (lambdag@ (st)
              (let ((st (state-with-scope st nonlocal-scope)))
                (let ((z ((reify q) st)))
                  (choice z empty-f)))))
          (empty-state)))))
    ((_ n (q0 q1 q ...) g0 g ...)
     (run n (x) (fresh (q0 q1 q ...) g0 g ... (== `(,q0 ,q1 ,q ...) x))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (q0 q ...) g0 g ...) (run #f (q0 q ...) g0 g ...))))

(define take
  (lambda (n f)
    (cond
      ((and n (zero? n)) '())
      (else
       (case-inf (f)
         (() '())
         ((f) (take n f))
         ((c) (cons c '()))
         ((c f) (cons c
                  (take (and n (- n 1)) f))))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (st)
       (inc (bind (state-depth-deepen (state-with-scope st (new-scope)))
                  (lambdag@ (st)
                    (mplus* (bind*-depth st g0 g ...)
                            (bind*-depth st g1 g^ ...) ...))))))))
(define-syntax conde-dfs
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (st)
       (inc (bind (state-depth-deepen (state-with-scope st (new-scope)))
                  (lambdag@ (st)
                    (mplus*-dfs (bind*-depth st g0 g ...)
                                (bind*-depth st g1 g^ ...) ...))))))))
(define-syntax conde-weighted
  (syntax-rules ()
    ((_ (w0 c0 g0 g ...) (w1 c1 g1 g^ ...) ...)
     (lambdag@ (st)
       (inc (bind (state-depth-deepen (state-with-scope st (new-scope)))
                  (lambdag@ (st)
                    (mplus*-weighted
                      (w0 c0 (bind*-depth st g0 g ...))
                      (w1 c1 (bind*-depth st g1 g^ ...)) ...))))))))

(define-syntax conde$
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (st)
       (let ((st (state-with-scope st (new-scope))))
         (mplus*
           (bind*-depth st g0 g ...)
           (bind*-depth st g1 g^ ...) ...))))))
(define-syntax conde$-dfs
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (st)
       (let ((st (state-with-scope st (new-scope))))
         (mplus*-dfs
           (bind*-depth st g0 g ...)
           (bind*-depth st g1 g^ ...) ...))))))
(define-syntax conde$-weighted
  (syntax-rules ()
    ((_ (w0 c0 g0 g ...) (w1 c1 g1 g^ ...) ...)
     (lambdag@ (st)
       (let ((st (state-with-scope st (new-scope))))
         (mplus*-weighted
           (w0 c0 (bind*-depth st g0 g ...))
           (w1 c1 (bind*-depth st g1 g^ ...)) ...))))))

(define-syntax case0
  (syntax-rules ()
    ((_ expr zero ((cs) more))
     (let ((result expr)) (if result (let ((cs result)) more) zero)))))

(define-syntax mplus1*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...)
     (case0 e0
       (mplus1* e ...)
       ((c0) (mplus0* c0 e ...))))))

(define-syntax mplus0*
  (syntax-rules ()
    ((_ unpruned) (if (procedure? unpruned) (unpruned) unpruned))
    ((_ unpruned e0 e ...)
     (case0 e0
       (mplus0* unpruned e ...)
       ((c0) (inc (mplus unpruned (inc (mplus* c0 e ...)))))))))

(define-syntax let**
  (syntax-rules ()
    ((_ () body) body)
    ((_ (((name val) ...) ... ((name1 val1) ...)) body ...)
     (let** (((name val) ...) ...) (let* ((name1 val1) ...) body ...)))))

(define-syntax conde1^
  (syntax-rules ()
    ((_ conde^ (((name lvar) ...) ...) (g0 g ...) ...)
     (lambdag@ (st)
       (let** (((name (walk lvar (state-S st))) ...) ...
               ((depth (state-depth st))
                (goal (lambdag@ (st) ((conde^ (g0 g ...) ...)
                                      (state-depth-set st depth))))
                (goal-deterministic
                  (lambdag@ (st) ((conde$ (g0 g ...) ...)
                                  (state-depth-set st depth))))))
         (if (and (ormap var? (list name ...)) ...)
           (state-deferred-defer st goal)
           (goal-deterministic st)))))))
(define-syntax conde1
  (syntax-rules () ((_ body ...) (conde1^ conde body ...))))
(define-syntax conde1$
  (syntax-rules () ((_ body ...) (conde1^ conde$ body ...))))

(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...) (mplus e0
                    (inc (mplus* e ...))))))
(define-syntax mplus*-dfs
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...) (mplus-dfs e0
                    (inc (mplus*-dfs e ...))))))
(define-syntax mplus*-weighted
  (syntax-rules ()
    ((_ (_ _ e)) e)
    ((_ (w0 c0 e0) e ...)
     (mplus-weighted w0 w0 1 c0 e0 (inc (mplus*-weighted e ...))))))

(define mplus
  (lambda (c-inf f)
    (case-inf c-inf
      (() (f))
      ((f^) (inc (mplus (f) f^)))
      ((c) (choice c f))
      ((c f^) (choice c (inc (mplus (f) f^)))))))

(define mplus-dfs
  (lambda (c-inf f)
    (case-inf c-inf
      (() (f))
      ((f^) (inc (mplus-dfs (f^) f)))
      ((c) (choice c f))
      ((c f^) (choice c (inc (mplus-dfs (f^) f)))))))

(define (mplus-weighted-next remaining weight other cycles f^ f)
  (inc (let ((weight-next (and remaining (- remaining 1))))
         (if (and weight-next (<= weight-next 0))
           (let ((cycles (and cycles (- cycles 1))))
             (if (and cycles (<= cycles 0))
               (mplus (f) f^)
               (mplus-weighted other other weight cycles (f) f^)))
           (mplus-weighted weight-next weight other cycles (f^) f)))))
(define mplus-weighted
  (lambda (remaining weight other-weight cycles c-inf f)
    (case-inf c-inf
      (() (f))
      ((f^) (inc (mplus-weighted-next
                   remaining weight other-weight cycles f^ f)))
      ((c) (choice c f))
      ((c f^) (choice c (inc (mplus-weighted-next
                               remaining weight other-weight cycles f^ f)))))))

; Constraints
; C refers to the constraint store map
; c refers to an individual constraint record


; Requirements for type constraints:
; 1. Must be positive, not negative. not-pairo wouldn't work.
; 2. Each type must have infinitely many possible values to avoid
;      incorrectness in combination with disequality constraints, like:
;      (fresh (x) (booleano x) (=/= x #t) (=/= x #f))
(define type-constraint
  (lambda (type-pred type-id)
    (lambda (u)
      (lambdag@ (st)
        (let ((term (walk u (state-S st))))
          (cond
            ((type-pred term) (unit st))
            ((var? term)
             (let* ((c (lookup-c term st))
                   (T (c-T c)))
               (cond
                 ((eq? T type-id) (unit st))
                 ((not T) (unit (set-c term (c-with-T c type-id) st)))
                 (else (mzero)))))
            (else (mzero))))))))

(define symbolo (type-constraint symbol? 'symbolo))
(define numbero (type-constraint number? 'numbero))

(define (add-to-D st v d)
  (let* ((c (lookup-c v st))
         (c^ (c-with-D c (cons d (c-D c)))))
    (set-c v c^ st)))

(define =/=*
  (lambda (S+)
    (lambdag@ (st)
      (let-values (((S added) (unify* S+ (subst-with-scope (state-S st) nonlocal-scope))))
        (cond
          ((not S) (unit st))
          ((null? added) (mzero))
          (else
            ; Choose one of the disequality elements (el) to attach the constraint to. Only
            ; need to choose one because all must fail to cause the constraint to fail.
            (let ((el (car added)))
              (let ((st (add-to-D st (car el) added)))
                (if (var? (cdr el))
                  (add-to-D st (cdr el) added)
                  st)))))))))

(define =/=
  (lambda (u v)
    (=/=* `((,u . ,v)))))

(define absento
  (lambda (ground-atom term)
    (lambdag@ (st)
      (let ((term (walk term (state-S st))))
        (cond
          ((pair? term)
           (let ((st^ ((absento ground-atom (car term)) st)))
             (and st^ ((absento ground-atom (cdr term)) st^))))
          ((eqv? term ground-atom) (mzero))
          ((var? term)
           (let* ((c (lookup-c term st))
                  (A (c-A c)))
             (if (memv ground-atom A)
               (unit st)
               (let ((c^ (c-with-A c (cons ground-atom A))))
                 (unit (set-c term c^ st))))))
          (else (unit st)))))))

; Fold lst with proc and initial value init. If proc ever returns #f,
; return with #f immediately. Used for applying a series of constraints
; to a state, failing if any operation fails.
(define (and-foldl proc init lst)
  (if (null? lst)
    init
    (let ([res (proc (car lst) init)])
      (and res (and-foldl proc res (cdr lst))))))

(define ==
  (lambda (u v)
    (lambdag@ (st)
      (let-values (((S added) (unify u v (state-S st))))
        (if S
          (and-foldl
            update-constraints
            (state S (state-C st) (state-depth st) (state-deferred st)) added)
          (mzero))))))


; Not fully optimized. Could do absento update with fewer hash-refs / hash-sets.
(define update-constraints
  (lambda (a st)
    (let ([old-c (lookup-c (lhs a) st)])
      (if (eq? old-c empty-c)
        st
        (let ((st (remove-c (lhs a) st)))
         (and-foldl (lambda (op st) (op st)) st
          (append
            (if (eq? (c-T old-c) 'symbolo)
              (list (symbolo (rhs a)))
              '())
            (if (eq? (c-T old-c) 'numbero)
              (list (numbero (rhs a)))
              '())
            (map (lambda (atom) (absento atom (rhs a))) (c-A old-c))
            (map (lambda (d) (=/=* d)) (c-D old-c)))))))))


; Reification

(define walk*
  (lambda (v S)
    (let ((v (walk v S)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons (walk* (car v) S) (walk* (cdr v) S)))
        (else v)))))

(define vars
  (lambda (term acc)
    (cond
      ((var? term) (cons term acc))
      ((pair? term)
       (vars (cdr term) (vars (car term) acc)))
      (else acc))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambdag@ (st)
       (let ((x (walk* x (state-S st))) ...)
         ((fresh () g g* ...) st))))))

(define-syntax project0
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambdag@ (st)
       (let ((x (walk x (state-S st))) ...)
         ((fresh () g g* ...) st))))))

; Create a constraint store of the old representation from a state object,
; so that we can use the old reifier. Only accumulates constraints related
; to the variable being reified which makes things a bit faster.
(define c-from-st
  (lambda (st x)
    (let ((vs (vars (walk* x (state-S st)) '())))
      (foldl
        (lambda (v c-store)
          (let ((c (lookup-c v st)))
            (let ((S (state-S st))
                  (D (c->D c-store))
                  (Y (c->Y c-store))
                  (N (c->N c-store))
                  (T (c->T c-store))
                  (T^ (c-T c))
                  (D^ (c-D c))
                  (A^ (c-A c)))
              `(,S
                 ,(append D^ D)
                 ,(if (eq? T^ 'symbolo)
                    (cons v Y)
                    Y)
                 ,(if (eq? T^ 'numbero)
                    (cons v N)
                    N)
                 ,(append
                    (map (lambda (atom) (cons atom v)) A^)
                    T)))))
        `(,(state-S st) () () () ())
        (remove-duplicates vs)))))

(define reify
  (lambda (x)
    (lambda (st)
      (let ((c (c-from-st st x)))
        (let ((c (cycle c)))
          (let* ((S (c->S c))
                 (D (walk* (c->D c) S))
                 (Y (walk* (c->Y c) S))
                 (N (walk* (c->N c) S))
                 (T (walk* (c->T c) S)))
            (let ((v (walk* x S)))
              (let ((R (reify-S v (subst empty-subst-map nonlocal-scope))))
                (reify+ v R
                        (let ((D (remp
                                   (lambda (d)
                                     (let ((dw (walk* d S)))
                                       (anyvar? dw R)))
                                   (rem-xx-from-d c))))
                          (rem-subsumed D))
                        (remp
                          (lambda (y) (var? (walk y R)))
                          Y)
                        (remp
                          (lambda (n) (var? (walk n R)))
                          N)
                        (remp (lambda (t)
                                (anyvar? t R)) T))))))))))


; Bits from the old constraint implementation, still used for reification.

; In this part of the code, c refers to the
; old constraint store with components:
; S - substitution
; D - disequality constraints
; Y - symbolo
; N - numbero
; T - absento

(define c->S (lambda (c) (car c)))
(define c->D (lambda (c) (cadr c)))
(define c->Y (lambda (c) (caddr c)))
(define c->N (lambda (c) (cadddr c)))
(define c->T (lambda (c) (cadddr (cdr c))))

(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (st)
       ;; is 'state-with-scope' required here, as it is with conde?
       ;; I don't think so.
       (inc
        (ifa ((g0 st) g ...)
             ((g1 st) g^ ...) ...))))))

(define-syntax ifa
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((c-inf e))
       (case-inf c-inf
         (() (ifa b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* c-inf g ...))
         ((a f) (bind* c-inf g ...)))))))

(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (st)
       ;; is 'state-with-scope' required here, as it is with conde?
       ;; I don't think so.
       (inc
        (ifu ((g0 st) g ...)
             ((g1 st) g^ ...) ...))))))

(define-syntax ifu
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((c-inf e))
       (case-inf c-inf
         (() (ifu b ...))
         ((f) (inc (loop (f))))
         ((c) (bind* c-inf g ...))
         ((c f) (bind* (unit c) g ...)))))))

; Syntax for reification goal objects using the old constraint store
(define-syntax lambdar@
  (syntax-rules (:)
    ((_ (c) e) (lambda (c) e))
    ((_ (c : S D Y N T) e)
     (lambda (c)
       (let ((S (c->S c)) (D (c->D c)) (Y (c->Y c)) (N (c->N c)) (T (c->T c)))
         e)))))

(define tagged?
  (lambda (S Y y^)
    (exists (lambda (y) (eqv? (walk y S) y^)) Y)))

(define untyped-var?
  (lambda (S Y N t^)
    (let ((in-type? (lambda (y) (var-eq? (walk y S) t^))))
      (and (var? t^)
           (not (exists in-type? Y))
           (not (exists in-type? N))))))

(define reify-S
  (lambda (v S)
    (let ((v (walk v S)))
      (cond
        ((var? v)
         (let ((n (subst-length S)))
           (let ((name (reify-name n)))
             (subst-add S v name))))
        ((pair? v)
         (let ((S (reify-S (car v) S)))
           (reify-S (cdr v) S)))
        (else S)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define drop-dot
  (lambda (X)
    (map (lambda (t)
           (let ((a (lhs t))
                 (d (rhs t)))
             `(,a ,d)))
         X)))

(define sorter
  (lambda (ls)
    (list-sort lex<=? ls)))

(define lex<=?
  (lambda (x y)
    (string<=? (datum->string x) (datum->string y))))

(define datum->string
  (lambda (x)
    (call-with-string-output-port
      (lambda (p) (display x p)))))

;;; WEB -- 28 June 2016 -- return #t if walk*'d term t contains a gensym
;;; (Barliman hack to make the reified answers more readable.)
(define anygen?
  (lambda (t)
    (cond
      ((pair? t)
       (or (anygen? (car t))
           (anygen? (cdr t))))
      (else (gensym? t)))))

(define anyvar?
  (lambda (u r)
    (cond
      ((pair? u)
       (or (anyvar? (car u) r)
           (anyvar? (cdr u) r)))
      (else (var? (walk u r))))))

(define member*
  (lambda (u v)
    (cond
      ((equal? u v) #t)
      ((pair? v)
       (or (member* u (car v)) (member* u (cdr v))))
      (else #f))))

(define drop-N-b/c-const
  (lambdar@ (c : S D Y N T)
    (let ((const? (lambda (n)
                    (not (var? (walk n S))))))
      (cond
        ((find const? N) =>
           (lambda (n) `(,S ,D ,Y ,(remq1 n N) ,T)))
        (else c)))))

(define drop-Y-b/c-const
  (lambdar@ (c : S D Y N T)
    (let ((const? (lambda (y)
                    (not (var? (walk y S))))))
      (cond
        ((find const? Y) =>
           (lambda (y) `(,S ,D ,(remq1 y Y) ,N ,T)))
        (else c)))))

(define remq1
  (lambda (elem ls)
    (cond
      ((null? ls) '())
      ((eq? (car ls) elem) (cdr ls))
      (else (cons (car ls) (remq1 elem (cdr ls)))))))

(define same-var?
  (lambda (v)
    (lambda (v^)
      (and (var? v) (var? v^) (var-eq? v v^)))))

(define find-dup
  (lambda (f S)
    (lambda (set)
      (let loop ((set^ set))
        (cond
          ((null? set^) #f)
          (else
           (let ((elem (car set^)))
             (let ((elem^ (walk elem S)))
               (cond
                 ((find (lambda (elem^^)
                          ((f elem^) (walk elem^^ S)))
                        (cdr set^))
                  elem)
                 (else (loop (cdr set^))))))))))))

(define drop-N-b/c-dup-var
  (lambdar@ (c : S D Y N T)
    (cond
      (((find-dup same-var? S) N) =>
       (lambda (n) `(,S ,D ,Y ,(remq1 n N) ,T)))
      (else c))))

(define drop-Y-b/c-dup-var
  (lambdar@ (c : S D Y N T)
    (cond
      (((find-dup same-var? S) Y) =>
       (lambda (y)
         `(,S ,D ,(remq1 y Y) ,N ,T)))
      (else c))))

(define var-type-mismatch?
  (lambda (S Y N t1^ t2^)
    (cond
      ((num? S N t1^) (not (num? S N t2^)))
      ((sym? S Y t1^) (not (sym? S Y t2^)))
      (else #f))))

(define term-ununifiable?
  (lambda (S Y N t1 t2)
    (let ((t1^ (walk t1 S))
          (t2^ (walk t2 S)))
      (cond
        ((or (untyped-var? S Y N t1^) (untyped-var? S Y N t2^)) #f)
        ((var? t1^) (var-type-mismatch? S Y N t1^ t2^))
        ((var? t2^) (var-type-mismatch? S Y N t2^ t1^))
        ((and (pair? t1^) (pair? t2^))
         (or (term-ununifiable? S Y N (car t1^) (car t2^))
             (term-ununifiable? S Y N (cdr t1^) (cdr t2^))))
        (else (not (eqv? t1^ t2^)))))))

(define T-term-ununifiable?
  (lambda (S Y N)
    (lambda (t1)
      (let ((t1^ (walk t1 S)))
        (letrec
            ((t2-check
              (lambda (t2)
                (let ((t2^ (walk t2 S)))
                  (if (pair? t2^)
                    (and
                       (term-ununifiable? S Y N t1^ t2^)
                       (t2-check (car t2^))
                       (t2-check (cdr t2^)))
                    (term-ununifiable? S Y N t1^ t2^))))))
          t2-check)))))

(define num?
  (lambda (S N n)
    (let ((n (walk n S)))
      (cond
        ((var? n) (tagged? S N n))
        (else (number? n))))))

(define sym?
  (lambda (S Y y)
    (let ((y (walk y S)))
      (cond
        ((var? y) (tagged? S Y y))
        (else (symbol? y))))))

(define drop-T-b/c-Y-and-N
  (lambdar@ (c : S D Y N T)
    (let ((drop-t? (T-term-ununifiable? S Y N)))
      (cond
        ((find (lambda (t) ((drop-t? (lhs t)) (rhs t))) T) =>
         (lambda (t) `(,S ,D ,Y ,N ,(remq1 t T))))
        (else c)))))

(define move-T-to-D-b/c-t2-atom
  (lambdar@ (c : S D Y N T)
    (cond
      ((exists (lambda (t)
               (let ((t2^ (walk (rhs t) S)))
                 (cond
                   ((and (not (untyped-var? S Y N t2^))
                         (not (pair? t2^)))
                    (let ((T (remq1 t T)))
                      `(,S ((,t) . ,D) ,Y ,N ,T)))
                   (else #f))))
             T))
      (else c))))

(define terms-pairwise=?
  (lambda (pr-a^ pr-d^ t-a^ t-d^ S)
    (or
     (and (term=? pr-a^ t-a^ S)
          (term=? pr-d^ t-a^ S))
     (and (term=? pr-a^ t-d^ S)
          (term=? pr-d^ t-a^ S)))))

(define T-superfluous-pr?
  (lambda (S Y N T)
    (lambda (pr)
      (let ((pr-a^ (walk (lhs pr) S))
            (pr-d^ (walk (rhs pr) S)))
        (cond
          ((exists
               (lambda (t)
                 (let ((t-a^ (walk (lhs t) S))
                       (t-d^ (walk (rhs t) S)))
                   (terms-pairwise=? pr-a^ pr-d^ t-a^ t-d^ S)))
             T)
           (for-all
            (lambda (t)
              (let ((t-a^ (walk (lhs t) S))
                    (t-d^ (walk (rhs t) S)))
                (or
                 (not (terms-pairwise=? pr-a^ pr-d^ t-a^ t-d^ S))
                 (untyped-var? S Y N t-d^)
                 (pair? t-d^))))
            T))
          (else #f))))))

(define drop-from-D-b/c-T
  (lambdar@ (c : S D Y N T)
    (cond
      ((find
           (lambda (d)
             (exists
                 (T-superfluous-pr? S Y N T)
               d))
         D) =>
         (lambda (d) `(,S ,(remq1 d D) ,Y ,N ,T)))
      (else c))))

(define drop-t-b/c-t2-occurs-t1
  (lambdar@ (c : S D Y N T)
    (cond
      ((find (lambda (t)
               (let ((t-a^ (walk (lhs t) S))
                     (t-d^ (walk (rhs t) S)))
                 (mem-check t-d^ t-a^ S)))
             T) =>
             (lambda (t)
               `(,S ,D ,Y ,N ,(remq1 t T))))
      (else c))))

(define split-t-move-to-d-b/c-pair
  (lambdar@ (c : S D Y N T)
    (cond
      ((exists
         (lambda (t)
           (let ((t2^ (walk (rhs t) S)))
             (cond
               ((pair? t2^) (let ((ta `(,(lhs t) . ,(car t2^)))
                                  (td `(,(lhs t) . ,(cdr t2^))))
                              (let ((T `(,ta ,td . ,(remq1 t T))))
                                `(,S ((,t) . ,D) ,Y ,N ,T))))
               (else #f))))
         T))
      (else c))))

(define find-d-conflict
  (lambda (S Y N)
    (lambda (D)
      (find
       (lambda (d)
	 (exists (lambda (pr)
		   (term-ununifiable? S Y N (lhs pr) (rhs pr)))
		 d))
       D))))

(define drop-D-b/c-Y-or-N
  (lambdar@ (c : S D Y N T)
    (cond
      (((find-d-conflict S Y N) D) =>
       (lambda (d) `(,S ,(remq1 d D) ,Y ,N ,T)))
      (else c))))

(define cycle
  (lambdar@ (c)
    (let loop ((c^ c)
               (fns^ (LOF))
               (n (length (LOF))))
      (cond
        ((zero? n) c^)
        ((null? fns^) (loop c^ (LOF) n))
        (else
         (let ((c^^ ((car fns^) c^)))
           (cond
             ((not (eq? c^^ c^))
              (loop c^^ (cdr fns^) (length (LOF))))
             (else (loop c^ (cdr fns^) (sub1 n))))))))))

(define mem-check
  (lambda (u t S)
    (let ((t (walk t S)))
      (cond
        ((pair? t)
         (or (term=? u t S)
             (mem-check u (car t) S)
             (mem-check u (cdr t) S)))
        (else (term=? u t S))))))

(define term=?
  (lambda (u t S)
    (let-values (((S added) (unify u t (subst-with-scope S nonlocal-scope))))
      (and S (null? added)))))

(define ground-non-<type>?
  (lambda (pred)
    (lambda (u S)
      (let ((u (walk u S)))
        (cond
          ((var? u) #f)
          (else (not (pred u))))))))

(define ground-non-symbol?
  (ground-non-<type>? symbol?))

(define ground-non-number?
  (ground-non-<type>? number?))

(define succeed (== #f #f))

(define fail (== #f #t))

(define (onceo g)
  (condu
    (g succeed)))

(define ==fail-check
  (lambda (S0 D Y N T)
    (let ([S0 (subst-with-scope S0 nonlocal-scope)])
      (cond
        ((atomic-fail-check S0 Y ground-non-symbol?) #t)
        ((atomic-fail-check S0 N ground-non-number?) #t)
        ((symbolo-numbero-fail-check S0 Y N) #t)
        ((=/=-fail-check S0 D) #t)
        ((absento-fail-check S0 T) #t)
        (else #f)))))

(define atomic-fail-check
  (lambda (S A pred)
    (exists (lambda (a) (pred (walk a S) S)) A)))

(define symbolo-numbero-fail-check
  (lambda (S A N)
    (let ((N (map (lambda (n) (walk n S)) N)))
      (exists (lambda (a) (exists (same-var? (walk a S)) N))
        A))))

(define absento-fail-check
  (lambda (S T)
    (exists (lambda (t) (mem-check (lhs t) (rhs t) S)) T)))

(define =/=-fail-check
  (lambda (S D)
    (exists (d-fail-check S) D)))

(define d-fail-check
  (lambda (S)
    (lambda (d)
      (let-values (((S added) (unify* d S)))
        (and S (null? added))))))

(define reify+
  (lambda (v R D Y N T)
    (form (walk* v R)
          (walk* D R)
          (walk* Y R)
          (walk* N R)
          (rem-subsumed-T (walk* T R)))))

;;; WEB -- 28 June 2016 -- ensure reified value is always a list of
;;; term followed by (potentially empty) list of constraints
(define form
  (lambda (v D Y N T)
    (let ((fd (sort-D D))
          (fy (sorter Y))
          (fn (sorter N))
          (ft (sorter T)))
      (let ((fd (if (null? fd) fd
                    (let ((fd (drop-dot-D fd)))
                      `((=/= . ,fd)))))
            (fy (if (null? fy) fy `((sym . ,fy))))
            (fn (if (null? fn) fn `((num . ,fn))))
            (ft (if (null? ft) ft
                    (let ((ft (drop-dot ft)))
                      `((absento . ,ft))))))
        (append `(,v) (filter-gensyms (append fd fn fy ft)))))))

;;; WEB -- 27 Oct 2016 -- (hopefully) fixed code that removes constraints containing gensyms
;;; Must make sure we only remove those portions of a constraint that actually contain gensyms!
;;; We don't need to worry about symbolo or numbero constraints, since gensyms are symbols!
(define filter-gensyms
  (lambda (loc)
    (cond
      ((null? loc) '())
      ((or (eqv? (caar loc) '=/=)
           (eqv? (caar loc) 'absento))
       (let ((fc (filter (lambda (c) (not (anygen? c))) (cdar loc))))
         (if (null? fc)
             (filter-gensyms (cdr loc))
             (cons (cons (caar loc) fc)
                   (filter-gensyms (cdr loc))))))
      (else (cons (car loc) (filter-gensyms (cdr loc)))))))

#|
(define form
  (lambda (v D Y N T)
    (let ((fd (sort-D D))
          (fy (sorter Y))
          (fn (sorter N))
          (ft (sorter T)))
      (let ((fd (if (null? fd) fd
                    (let ((fd (drop-dot-D fd)))
                      `((=/= . ,fd)))))
            (fy (if (null? fy) fy `((sym . ,fy))))
            (fn (if (null? fn) fn `((num . ,fn))))
            (ft (if (null? ft) ft
                    (let ((ft (drop-dot ft)))
                      `((absento . ,ft))))))
        (cond
          ((and (null? fd) (null? fy)
                (null? fn) (null? ft))
           v)
          (else (append `(,v) fd fn fy ft)))))))
|#

(define sort-D
  (lambda (D)
    (sorter
     (map sort-d D))))

(define sort-d
  (lambda (d)
    (list-sort
       (lambda (x y)
         (lex<=? (car x) (car y)))
       (map sort-pr d))))

(define drop-dot-D
  (lambda (D)
    (map drop-dot D)))

(define lex<-reified-name?
  (lambda (r)
    (char<?
     (string-ref
      (datum->string r) 0)
     #\_)))

(define sort-pr
  (lambda (pr)
    (let ((l (lhs pr))
          (r (rhs pr)))
      (cond
        ((lex<-reified-name? r) pr)
        ((lex<=? r l) `(,r . ,l))
        (else pr)))))

(define rem-subsumed
  (lambda (D)
    (let rem-subsumed ((D D) (d^* '()))
      (cond
        ((null? D) d^*)
        ((or (subsumed? (car D) (cdr D))
             (subsumed? (car D) d^*))
         (rem-subsumed (cdr D) d^*))
        (else (rem-subsumed (cdr D)
                (cons (car D) d^*)))))))

(define subsumed?
  (lambda (d d*)
    (cond
      ((null? d*) #f)
      (else
        (let-values (((S ignore) (unify* d (subst empty-subst-map nonlocal-scope))))
          (let-values (((S+ added) (unify* (car d*) S)))
            (or
              (and S+ (null? added))
              (subsumed? d (cdr d*)))))))))



(define rem-xx-from-d
  (lambdar@ (c : S D Y N T)
    (let ((D (walk* D S)))
      (remp not
            (map (lambda (d)
                   (let-values (((S0 ignore) (unify* d S)))
                     (cond
                       ((not S0) #f)
                       ((==fail-check S0 '() Y N T) #f)
                       (else
                         (let-values (((S added) (unify* d (subst empty-subst-map nonlocal-scope))))
                           added)))))
                 D)))))

(define rem-subsumed-T
  (lambda (T)
    (let rem-subsumed ((T T) (T^ '()))
      (cond
        ((null? T) T^)
        (else
         (let ((lit (lhs (car T)))
               (big (rhs (car T))))
           (cond
             ((or (subsumed-T? lit big (cdr T))
                  (subsumed-T? lit big T^))
              (rem-subsumed (cdr T) T^))
             (else (rem-subsumed (cdr T)
                     (cons (car T) T^))))))))))

(define subsumed-T?
  (lambda (lit big T)
    (cond
      ((null? T) #f)
      (else
       (let ((lit^ (lhs (car T)))
             (big^ (rhs (car T))))
         (or
           (and (eq? big big^) (member* lit^ lit))
           (subsumed-T? lit big (cdr T))))))))

(define LOF
  (lambda ()
    `(,drop-N-b/c-const ,drop-Y-b/c-const ,drop-Y-b/c-dup-var
      ,drop-N-b/c-dup-var ,drop-D-b/c-Y-or-N ,drop-T-b/c-Y-and-N
      ,move-T-to-D-b/c-t2-atom ,split-t-move-to-d-b/c-pair
      ,drop-from-D-b/c-T ,drop-t-b/c-t2-occurs-t1)))

