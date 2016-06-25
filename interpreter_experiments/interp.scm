;; 2016 June 5 & 6

;; Abstract: In this file I experimented with a grammar relation for 'miniScheme'.  I also experimented with adding a "term size" counter to the grammar relation, to keep miniKanren's  biased search from spending most of its time generating 'lambda' expressions with 50 arguments rather than on much simpler terms more likely to be the result of an actual synthesis problem.  I had partial success.  If I were to keep pushing this experiment, I would probably try passing a term-size counter through 'eval-expr' monadically, rather that using a separate grammar or parsing goal.  Or, try using true IDDFS for the search.

;; Purpose:  I was hoping that adding an explicit grammar, especially one that enumerated terms in order of their size, could help with this simple synthesis problem that doesn't seem to come back, at least within several minutes:

;; (time (run 1 (q A B)
;;         (evalo `(begin
;;                   (define append
;;                     (lambda (l s)
;;                       (if (null? l)
;;                           s
;;                           (cons (car l)
;;                                 (append (,A ,B) s)))))
;;                   (list
;;                    (append '() '())
;;                    (append '(foo) '(bar))
;;                    (append '(a b c) '(d e))))
;;                '(() (foo bar) (a b c d e)))))

;; Similar looking problems, with A and B in other positions within 'append', often return within a few milliseconds.  Why is this problem so slow?  I suspect the slowness is due to A and B being within the "interesting" argument to the recursive call, athough I don't have a nuanced understanding of why this synthesis problem is so expensive.

;; The bigger problem, I think, is that miniKanren's interleaving search strategy results in ridiculous terms being considered, such as 'lambda' terms with dozens of formal paramaters, none of which are actually used.  I was hoping a grammar predicate/goal could help avoid this problem by enforcing a 'parsimony' property that is often used in synthesis problems, by considering smaller terms before larger terms.

;; Results:  I ended up inplementing a simple grammar goal with a size counter (represented as a Peano numeral) passed through monadically.  In hindsight, perhaps it would have been more useful to write a parser (which keeps tracks of variable binding and shadowing through an environment), onece again with a size counter passed through monadically.  This shouldn't be difficult, I think.  Even better, I think, might be passing the term-size counter through 'eval-expr' itself.  Or, try using true IDDFS for the search.

;; The grammar-with-monadic-counter approached seemed like a partial success.  I was able to write a query that explicitly used the grammar and counter to generate the desired term in 35 ms, and which can prove there are no other solutions of that size in about the same time::

;; > (time (run 1 (q A B)
;;           (grammar-with-sizeo `(,A ,B) '(s (s (s z))))
;;           (evalo `(begin
;;                     (define append
;;                       (lambda (l s)
;;                         (if (null? l)
;;                             s
;;                             (cons (car l)
;;                                   (append (,A ,B) s)))))
;;                     (append '(foo baz) '(bar)))
;;                  '(foo baz bar))))
;; (time (run 1 ...))
;;     3 collections
;;     0.035167000s elapsed cpu time, including 0.000336000s collecting
;;     0.035398000s elapsed real time, including 0.000353000s collecting
;;     25983792 bytes allocated, including 25256192 bytes reclaimed
;; ((_.0 cdr l))
;; > (time (run* (q A B)
;;           (grammar-with-sizeo `(,A ,B) '(s (s (s z))))
;;           (evalo `(begin
;;                     (define append
;;                       (lambda (l s)
;;                         (if (null? l)
;;                             s
;;                             (cons (car l)
;;                                   (append (,A ,B) s)))))
;;                     (append '(foo baz) '(bar)))
;;                  '(foo baz bar))))
;; (time (run* (q A ...) ...))
;;     4 collections
;;     0.037766000s elapsed cpu time, including 0.000397000s collecting
;;     0.037938000s elapsed real time, including 0.000413000s collecting
;;     27496656 bytes allocated, including 33834096 bytes reclaimed
;; ((_.0 cdr l))

;; There appear to be several issues with this approach.  First, I had to guess the term size, and supply it explicitly.  I think this problem is solveable using 'conda'/'condu' and recursion.  I also had to isolate the term in question, `(,A ,B), and feed that term to 'grammar-with-sizeo'.


;; One thing I realized is that the first attempt at a 'grammar' I was writing was actually more like a parser, since it was keeping track of which variables were bound in the environment, whether built-in forms like 'lambda' were being shadowed, etc.  Whether a parser or a simpler grammar is better, I'm not sure.  Given that we allow shadowing of built-in forms, though, it seems like a parser that tracks bindings in the environment is probably more useful, since otherwise it is impossible to determine if a "'lambda' expression" is instead an application, with the keyword 'lambda' being overriden in the outer lexical scope of the expression.

;; Anyway, the commented code, from the bottom of the file moving upwards, includes (if I recall correctly):
; * a parser;
; * a parser with a 'size' argument (represented as a Peano numeral: 'z, '(s z), '(s (s z)), etc.), with the 'size' argument passed through lexically;
; * a grammar (without the environment);
; * a grammar, with the 'size' argument passed through monadically.

;; I didn't really try a parser with a 'size' argument passed through monadically.  This would probably be the most useful version, I suspect.





;; We might need 4 levels of specification for Barliman, provided that we can get conjunction and search working decently:  grammatically valid, parses (wrt initial env), type checks/infers, evaluates to correct value

;; grammar

; number
; #t | #f
; symbol
; (quote datum)
; (lambda x e)
; (lambda (x ...) e)
;; match (not yet implemented)
;(begin (define args e) e) => letrec
;(letrec ((x (lambda x e))) e)
;(letrec ((x (lambda (x ...) e))) e)
; (e e* ...) ;; this overlaps with the others...

(define run-my-query
  (lambda ()
    (run 1 (expr)
      (fresh (A B C D E F)
        (== `(begin
               (define append
                 (lambda (l s)
                   (if (null? l)
                       s
                       (cons (car l)
                             (append (cdr l) s)))))
               (list (append '() '())))
            expr)
        (evalo expr (list '()))
        (go expr)))))

;;; If I can figure out how to properly use the grammar with a
;;; counter, I should probably integrate the counter directly into
;;; evalo

;;; How to get go to terminate with a ground expr?  Currently go-aux
;;; just keeps growing the max size value, which means that
;;; (run 2 (q) (go '3)) diverges.

;;; Maybe use Oleg numbers so I can use <o and <=o
(define (go expr)
  (go-aux expr 'z))

(define (go-aux expr size)
  (conde
    ((grammar-with-sizeo expr size))
    ((go-aux expr `(s ,size)))))

(define (grammar-with-sizeo expr size)
  (grammaro expr size 'z))

(define (grammaro expr size-in size-out)
  (conde
    ((== `(s ,size-out) size-in)  ; base cases
     (conde
       ((numbero expr))
       ((== #t expr))
       ((== #f expr))
       ((symbolo expr))))

    ((fresh (size-2)  ; recursive cases
       (== `(s (s ,size-2)) size-in)
       (conde
         ((fresh (datum size-1)
            (== `(quote ,datum) expr)
            (== `(s ,size-1) size-in)
            (grammar-datumo datum size-1 size-out)))
         ((fresh (args e size-1)
            (== `(lambda ,args ,e) expr)
            (== `(s ,size-1) size-in)
            (conde
              ((symbolo args)
               (grammaro e size-1 size-out))
              ((fresh (size^)
                 (grammar-list-of-symbolso args size-1 size^)
                 (grammaro e size^ size-out))))))
         ;; TODO match
         ((fresh (defn e name args body size-1)
            (== `(begin ,defn ,e) expr)
            (== `(define ,name (lambda ,args ,body)) defn)
            (== `(s ,size-1) size-in) ;; is this necessary?
            (grammaro `(letrec ((,name (lambda ,args ,body))) ,e) size-in size-out)))
         ((fresh (p-name args body letrec-body size-1)
            (== `(letrec ((,p-name (lambda ,args ,body)))
                   ,letrec-body)
                expr)
            (== `(s ,size-1) size-in)
            (symbolo p-name)
            (conde
              ((symbolo args)
               (fresh (size^)
                 (grammaro body size-1 size^)
                 (grammaro letrec-body size^ size-out)))
              ((fresh (size^ size^^)
                 (grammar-list-of-symbolso args size-1 size^)
                 (grammaro body size^ size^^)
                 (grammaro letrec-body size^^ size-out))))))
         ((fresh (e e* size-1)
            (== `(,e . ,e*) expr)
            (== `(s ,size-1) size-in) ;; is this necessary?
            (grammar-listo `(,e . ,e*) size-1 size-out)))
         )))))

(define (grammar-datumo datum size-in size-out)
  (conde
    ((== `(s ,size-out) size-in)        ; base cases
     (conde
       ((== '() datum))
       ((numbero datum))
       ((== #t datum))
       ((== #f datum))
       ((symbolo datum))))
    ((fresh (a d size-1 size-2 size^)                    ; recursive cases
       (== `(,a . ,d) datum)
       (== `(s (s ,size-2)) size-in)
       (== `(s ,size-1) size-in)
       (grammar-datumo a size-1 size^)
       (grammar-datumo d size^ size-out)))))

(define (grammar-list-of-symbolso los size-in size-out)
  (conde
    ((== '() los)
     (== size-in size-out))
    ((fresh (x rest size-1)
       (== `(,x . ,rest) los)
       (== `(s ,size-1) size-in)
       (symbolo x)
       (grammar-list-of-symbolso rest size-1 size-out)))))

(define (grammar-listo e* size-in size-out)
  (conde
    ((== '() e*)
     (== size-in size-out))
    ((fresh (e rest size^)
       (== `(,e . ,rest) e*)
       (grammaro e size-in size^)
       (grammar-listo rest size^ size-out)))))




;; (define (grammaro expr)
;;   (conde
;;     ((numbero expr))
;;     ((== #t expr))
;;     ((== #f expr))
;;     ((symbolo expr))
;;     ((fresh (datum)
;;        (== `(quote ,datum) expr)))
;;     ((fresh (args e)
;;        (== `(lambda ,args ,e) expr)
;;        (conde
;;          ((symbolo args))
;;          ((list-of-symbolso args)))
;;        (grammaro e)))
;;     ;; TODO match
;;     ((fresh (defn e name args body)
;;        (== `(begin ,defn ,e) expr)
;;        (== `(define ,name (lambda ,args ,body)) defn)
;;        (grammaro `(letrec ((,name (lambda ,args ,body))) ,e))))
;;     ((fresh (p-name args body letrec-body)
;;        (== `(letrec ((,p-name (lambda ,args ,body)))
;;               ,letrec-body)
;;            expr)
;;        (symbolo p-name)
;;        (conde
;;          ((symbolo args))
;;          ((list-of-symbolso args)))
;;        (grammaro body)
;;        (grammaro letrec-body)))
;;     ((fresh (e e*)
;;        (== `(,e . ,e*) expr)
;;        (grammar-listo `(,e . ,e*))))))

;; (define (list-of-symbolso los)
;;   (conde
;;     ((== '() los))
;;     ((fresh (x rest)
;;        (== `(,x . ,rest) los)
;;        (symbolo x)
;;        (list-of-symbolso rest)))))

;; (define (grammar-listo e*)
;;   (conde
;;     ((== '() e*))
;;     ((fresh (e rest)
;;        (== `(,e . ,rest) e*)
;;        (grammaro e)
;;        (grammar-listo rest)))))





;; ;; grammar with size (as a peano numeral: z, (s z), (s (s z)))
;; (define (grammaro expr size)
;;   (grammar-expo expr initial-env size))

;; (define (grammar-expo expr env size)
;;   (conde

;;     ((== 'z size) ;; base cases
;;      (conde
;;        ((numbero expr))

;;        ((fresh (val)
;;           (== `(quote ,val) expr)
;;           (absento 'closure val)
;;           (absento 'prim val)
;;           (not-in-envo 'quote env)))

;;        ((symbolo expr)
;;         (fresh (val)
;;           (lookupo expr env val)))))


;;     ((fresh (size-1) ;; recursive cases
;;        (== `(s ,size-1) size)

;;        (conde

;;          ((fresh (x body)
;;             (== `(lambda ,x ,body) expr)
;;             (not-in-envo 'lambda env)
;;             (conde
;;               ;; Variadic
;;               ((symbolo x)
;;                (fresh (a env^)
;;                  (== `((,x . (val . ,a)) . ,env) env^)
;;                  (grammar-expo body env^ size-1)))
;;               ;; Multi-argument
;;               ((list-of-symbolso x)
;;                (fresh (a env^)
;;                  (ext-env*o x a env env^)
;;                  (grammar-expo body env^ size-1))))))

;;          ((fresh (defn args name body e)
;;             (== `(begin ,defn ,e) expr)
;;             (== `(define ,name (lambda ,args ,body)) defn)
;;             (grammar-expo `(letrec ((,name (lambda ,args ,body))) ,e) env size)))

;;          ((fresh (rator rands)
;;             (== `(,rator . ,rands) expr)
;;             ;; application
;;             (grammar-listo `(,rator . ,rands) env size)))

;;          ;; TODO
;;          ;;    ((grammar-matcho expr env size))

;;          ;; TODO -- might need to pass size through monadically to deal with the multiple recursive calls in
;;          ;; the letrec case, and in the grammar-listo case.
;;          ((fresh (p-name x body letrec-body)
;;             ;; single-function variadic letrec version
;;             (== `(letrec ((,p-name (lambda ,x ,body)))
;;                    ,letrec-body)
;;                 expr)
;;             (not-in-envo 'letrec env)
;;             (conde
;;                                         ; Variadic
;;               ((symbolo x)
;;                (fresh (a env^)
;;                  (== `((,x . (val . ,a)) . ,env) env^)
;;                  (grammar-expo body env^ size-1)))
;;                                         ; Multiple argument
;;               ((list-of-symbolso x)
;;                (fresh (a env^)
;;                  (ext-env*o x a env env^)
;;                  (grammar-expo body env^ size-1))))
;;             (grammar-expo letrec-body
;;                           `((,p-name . (rec . (lambda ,x ,body))) . ,env)
;;                           size-1)))
;;          )))

;;     ))

;; ;; TODO probably need to pass size through monadically
;; (define (grammar-listo expr env size)
;;   (conde
;;     ((== 'z size)
;;      (== '() expr))
;;     ((fresh (size-1)
;;        (== `(s ,size-1) size)
;;        (fresh (a d)
;;          (== `(,a . ,d) expr)
;;          (grammar-expo a env size-1)
;;          (grammar-listo d env size-1))))))



;; ;; This is more 'parseo' than 'grammaro'.  It tries to keep track of which variables are bound in the environment.
;; ;; grammar with size (as a peano numeral: z, (s z), (s (s z)))
;; (define (grammaro expr size)
;;   (grammar-expo expr initial-env size))

;; (define (grammar-expo expr env size)
;;   (conde

;;     ((== 'z size) ;; base cases
;;      (conde
;;        ((numbero expr))

;;        ((fresh (val)
;;           (== `(quote ,val) expr)
;;           (absento 'closure val)
;;           (absento 'prim val)
;;           (not-in-envo 'quote env)))

;;        ((symbolo expr)
;;         (fresh (val)
;;           (lookupo expr env val)))))


;;     ((fresh (size-1) ;; recursive cases
;;        (== `(s ,size-1) size)

;;        (conde

;;          ((fresh (x body)
;;             (== `(lambda ,x ,body) expr)
;;             (not-in-envo 'lambda env)
;;             (conde
;;               ;; Variadic
;;               ((symbolo x)
;;                (fresh (a env^)
;;                  (== `((,x . (val . ,a)) . ,env) env^)
;;                  (grammar-expo body env^ size-1)))
;;               ;; Multi-argument
;;               ((list-of-symbolso x)
;;                (fresh (a env^)
;;                  (ext-env*o x a env env^)
;;                  (grammar-expo body env^ size-1))))))

;;          ((fresh (defn args name body e)
;;             (== `(begin ,defn ,e) expr)
;;             (== `(define ,name (lambda ,args ,body)) defn)
;;             (grammar-expo `(letrec ((,name (lambda ,args ,body))) ,e) env size)))

;;          ((fresh (rator rands)
;;             (== `(,rator . ,rands) expr)
;;             ;; application
;;             (grammar-listo `(,rator . ,rands) env size)))

;;          ;; TODO
;;          ;;    ((grammar-matcho expr env size))

;;          ;; TODO -- might need to pass size through monadically to deal with the multiple recursive calls in
;;          ;; the letrec case, and in the grammar-listo case.
;;          ((fresh (p-name x body letrec-body)
;;             ;; single-function variadic letrec version
;;             (== `(letrec ((,p-name (lambda ,x ,body)))
;;                    ,letrec-body)
;;                 expr)
;;             (not-in-envo 'letrec env)
;;             (conde
;;                                         ; Variadic
;;               ((symbolo x)
;;                (fresh (a env^)
;;                  (== `((,x . (val . ,a)) . ,env) env^)
;;                  (grammar-expo body env^ size-1)))
;;                                         ; Multiple argument
;;               ((list-of-symbolso x)
;;                (fresh (a env^)
;;                  (ext-env*o x a env env^)
;;                  (grammar-expo body env^ size-1))))
;;             (grammar-expo letrec-body
;;                           `((,p-name . (rec . (lambda ,x ,body))) . ,env)
;;                           size-1)))
;;          )))

;;     ))

;; ;; TODO probably need to pass size through monadically
;; (define (grammar-listo expr env size)
;;   (conde
;;     ((== 'z size)
;;      (== '() expr))
;;     ((fresh (size-1)
;;        (== `(s ,size-1) size)
;;        (fresh (a d)
;;          (== `(,a . ,d) expr)
;;          (grammar-expo a env size-1)
;;          (grammar-listo d env size-1))))))














;; grammar without size

;; (define (grammaro expr)
;;   (grammar-expo expr initial-env))

;; (define (grammar-expo expr env)
;;   (conde
;;     ((fresh (val)
;;        (== `(quote ,val) expr)
;;        (absento 'closure val)
;;        (absento 'prim val)
;;        (not-in-envo 'quote env)))

;;     ((numbero expr))

;;     ((symbolo expr)
;;      (fresh (val)
;;        (lookupo expr env val)))

;; TODO -- fix me -- should be recursive!
;;     ((fresh (x body)
;;        (== `(lambda ,x ,body) expr)
;;        (not-in-envo 'lambda env)
;;        (conde
;;          ;; Variadic
;;          ((symbolo x))
;;          ;; Multi-argument
;;          ((list-of-symbolso x)))))

;;     ((fresh (defn args name body e)
;;        (== `(begin ,defn ,e) expr)
;;        (== `(define ,name (lambda ,args ,body)) defn)
;;        (grammar-expo `(letrec ((,name (lambda ,args ,body))) ,e) env)))

;;     ((fresh (rator rands)
;;        (== `(,rator . ,rands) expr)
;;        ;; application
;;        (grammar-listo `(,rator . ,rands) env)))

;; ;; TODO
;; ;;    ((grammar-matcho expr env))

;;     ((fresh (p-name x body letrec-body)
;;        ;; single-function variadic letrec version
;;        (== `(letrec ((,p-name (lambda ,x ,body)))
;;               ,letrec-body)
;;            expr)
;;        (not-in-envo 'letrec env)
;;        (conde
;;          ; Variadic
;;          ((symbolo x)
;;           (fresh (a env^)
;;             (== `((,x . (val . ,a)) . ,env) env^)
;;             (grammar-expo body env^)))
;;          ; Multiple argument
;;          ((list-of-symbolso x)
;;           (fresh (a env^)
;;             (ext-env*o x a env env^)
;;             (grammar-expo body env^))))
;;        (grammar-expo letrec-body
;;                      `((,p-name . (rec . (lambda ,x ,body))) . ,env))))

;;     ))

;; (define (grammar-listo expr env)
;;   (conde
;;     ((== '() expr))
;;     ((fresh (a d)
;;        (== `(,a . ,d) expr)
;;        (grammar-expo a env)
;;        (grammar-listo d env)))))



(define (evalo expr val)
  (eval-expo expr initial-env val))

(define (eval-expo expr env val)
  (conde

    ((== `(quote ,val) expr)
     (absento 'closure val)
     (absento 'prim val)
     (not-in-envo 'quote env))

    ((numbero expr) (== expr val))

    ((symbolo expr) (lookupo expr env val))

    ((fresh (x body)
       (== `(lambda ,x ,body) expr)
       (== `(closure (lambda ,x ,body) ,env) val)
       (not-in-envo 'lambda env)
       (conde
         ;; Variadic
         ((symbolo x))
         ;; Multi-argument
         ((list-of-symbolso x)))))

    ;; WEB 25 May 2016 -- This rather budget version of 'begin' is
    ;; useful for separating 'define' from the expression 'e',
    ;; specifically for purposes of Barliman.
    ((fresh (defn args name body e)
       (== `(begin ,defn ,e) expr)
       (== `(define ,name (lambda ,args ,body)) defn)
       (eval-expo `(letrec ((,name (lambda ,args ,body))) ,e) env val)))

    ((fresh (rator rands proc)
       (== `(,rator . ,rands) expr)
       (fresh (rest)
         (== `(closure . ,rest) proc))
       (eval-expo rator env proc)
       (conde
         ((fresh (args body env^ a* res)
            (== `(closure (lambda ,args ,body) ,env^) proc)
            (conde
              ((symbolo args)
               (== `((,args . (val . ,a*)) . ,env^) res)
               (eval-expo body res val)
               (eval-listo rands env a*))
              ((eval-listo rands env a*)
               (ext-env*o args a* env^ res)
               (eval-expo body res val)))))
         ((fresh (prim-id a*)
            ;; primitive
            (== `(closure prim . ,prim-id) proc)
            (eval-primo prim-id a* val)
            (eval-listo rands env a*))))))

    ;; ((fresh (rator x rands body env^ a* res)
    ;;    (== `(,rator . ,rands) expr)
    ;;    ;; variadic
    ;;    (symbolo x)
    ;;    (== `((,x . (val . ,a*)) . ,env^) res)
    ;;    (eval-expo rator env `(closure (lambda ,x ,body) ,env^))
    ;;    (eval-expo body res val)
    ;;    (eval-listo rands env a*)))

    ;; ((fresh (rator x* rands body env^ a* res)
    ;;    (== `(,rator . ,rands) expr)
    ;;    ;; Multi-argument
    ;;    (eval-expo rator env `(closure (lambda ,x* ,body) ,env^))
    ;;    (eval-listo rands env a*)
    ;;    (ext-env*o x* a* env^ res)
    ;;    (eval-expo body res val)))

    ;; ((fresh (rator x* rands a* prim-id)
    ;;    (== `(,rator . ,rands) expr)
    ;;    (eval-expo rator env `(prim . ,prim-id))
    ;;    (eval-primo prim-id a* val)
    ;;    (eval-listo rands env a*)))

    ((handle-matcho expr env val))

    ((fresh (p-name x body letrec-body)
       ;; single-function variadic letrec version
       (== `(letrec ((,p-name (lambda ,x ,body)))
              ,letrec-body)
           expr)
       (not-in-envo 'letrec env)
       (conde
         ; Variadic
         ((symbolo x))
         ; Multiple argument
         ((list-of-symbolso x)))
       (eval-expo letrec-body
                  `((,p-name . (rec . (lambda ,x ,body))) . ,env)
                  val)))

    ((prim-expo expr env val))

    ))

(define empty-env '())

(define (lookupo x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (conde
         ((== `(val . ,t) b))
         ((fresh (lam-expr)
            (== `(rec . ,lam-expr) b)
            (== `(closure ,lam-expr ,env) t)))))
      ((=/= x y)
       (lookupo x rest t)))))

(define (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))

(define (eval-listo expr env val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo a env v-a)
       (eval-listo d env v-d)))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(define (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(define (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

(define (eval-primo prim-id a* val)
  (conde
    [(== prim-id 'cons)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== `(,a . ,d) val))]
    [(== prim-id 'car)
     (fresh (d)
       (== `((,val . ,d)) a*)
       (=/= 'closure val))]
    [(== prim-id 'cdr)
     (fresh (a)
       (== `((,a . ,val)) a*)
       (=/= 'closure a))]
    [(== prim-id 'not)
     (fresh (b)
       (== `(,b) a*)
       (conde
         ((=/= #f b) (== #f val))
         ((== #f b) (== #t val))))]
    [(== prim-id 'equal?)
     (fresh (v1 v2)
       (== `(,v1 ,v2) a*)
       (conde
         ((== v1 v2) (== #t val))
         ((=/= v1 v2) (== #f val))))]
    [(== prim-id 'symbol?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((symbolo v) (== #t val))
         ((numbero v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)))))]
    [(== prim-id 'null?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((== '() v) (== #t val))
         ((=/= '() v) (== #f val))))]))

(define (prim-expo expr env val)
  (conde
    ((boolean-primo expr env val))
    ((and-primo expr env val))
    ((or-primo expr env val))
    ((if-primo expr env val))))

(define (boolean-primo expr env val)
  (conde
    ((== #t expr) (== #t val))
    ((== #f expr) (== #f val))))

(define (and-primo expr env val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (not-in-envo 'and env)
    (ando e* env val)))

(define (ando e* env val)
  (conde
    ((== '() e*) (== #t val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((== #f v)
          (== #f val)
          (eval-expo e1 env v))
         ((=/= #f v)
          (eval-expo e1 env v)
          (ando `(,e2 . ,e-rest) env val)))))))

(define (or-primo expr env val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (not-in-envo 'or env)
    (oro e* env val)))

(define (oro e* env val)
  (conde
    ((== '() e*) (== #f val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((=/= #f v)
          (== v val)
          (eval-expo e1 env v))
         ((== #f v)
          (eval-expo e1 env v)
          (oro `(,e2 . ,e-rest) env val)))))))

(define (if-primo expr env val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (eval-expo e1 env t)
    (conde
      ((=/= #f t) (eval-expo e2 env val))
      ((== #f t) (eval-expo e3 env val)))))

(define initial-env `((list . (val . (closure (lambda x x) ,empty-env)))
                      (not . (val . (closure prim . not)))
                      (equal? . (val . (closure prim . equal?)))
                      (symbol? . (val . (closure prim . symbol?)))
                      (cons . (val . (closure prim . cons)))
                      (null? . (val . (closure prim . null?)))
                      (car . (val . (closure prim . car)))
                      (cdr . (val . (closure prim . cdr)))
                      . ,empty-env))

(define handle-matcho
  (lambda  (expr env val)
    (fresh (against-expr mval clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (not-in-envo 'match env)
      (eval-expo against-expr env mval)
      (match-clauses mval `(,clause . ,clauses) env val))))

(define (not-symbolo t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (not-numbero t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (self-eval-literalo t)
  (conde
    ((numbero t))
    ((booleano t))))

(define (literalo t)
  (conde
    ((numbero t))
    ((symbolo t) (=/= 'closure t))
    ((booleano t))
    ((== '() t))))

(define (booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

(define (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

(define (match-clauses mval clauses env val)
  (fresh (p result-expr d penv)
    (== `((,p ,result-expr) . ,d) clauses)
    (conde
      ((fresh (env^)
         (p-match p mval '() penv)
         (regular-env-appendo penv env env^)
         (eval-expo result-expr env^ val)))
      ((p-no-match p mval '() penv)
       (match-clauses mval d env val)))))

(define (var-p-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= 'closure mval)
    (conde
      ((== mval val)
       (== penv penv-out)
       (lookupo var penv val))
      ((== `((,var . (val . ,mval)) . ,penv) penv-out)
       (not-in-envo var penv)))))

(define (var-p-no-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= mval val)
    (== penv penv-out)
    (lookupo var penv val)))

(define (p-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (== p mval)
     (== penv penv-out))
    ((var-p-match p mval penv penv-out))
    ((fresh (var pred val)
      (== `(? ,pred ,var) p)
      (conde
        ((== 'symbol? pred)
         (symbolo mval))
        ((== 'number? pred)
         (numbero mval)))
      (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-match quasi-p mval penv penv-out)))))

(define (p-no-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (=/= p mval)
     (== penv penv-out))
    ((var-p-no-match p mval penv penv-out))
    ((fresh (var pred val)
       (== `(? ,pred ,var) p)
       (== penv penv-out)
       (symbolo var)
       (conde
         ((== 'symbol? pred)
          (conde
            ((not-symbolo mval))
            ((symbolo mval)
             (var-p-no-match var mval penv penv-out))))
         ((== 'number? pred)
          (conde
            ((not-numbero mval))
            ((numbero mval)
             (var-p-no-match var mval penv penv-out)))))))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-no-match quasi-p mval penv penv-out)))))

(define (quasi-p-match quasi-p mval penv penv-out)
  (conde
    ((== quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
      (== (list 'unquote p) quasi-p)
      (p-match p mval penv penv-out)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (== `(,v1 . ,v2) mval)
       (=/= 'unquote a)
       (quasi-p-match a v1 penv penv^)
       (quasi-p-match d v2 penv^ penv-out)))))

(define (quasi-p-no-match quasi-p mval penv penv-out)
  (conde
    ((=/= quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (=/= 'closure mval)
       (p-no-match p mval penv penv-out)))
    ((fresh (a d)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== penv penv-out)
       (literalo mval)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== `(,v1 . ,v2) mval)
       (conde
         ((quasi-p-no-match a v1 penv penv^))
         ((quasi-p-match a v1 penv penv^)
          (quasi-p-no-match d v2 penv^ penv-out)))))))

;; TODO
;;
;; add multi-function letrec
;;
;; make begin support multiple definitions
;;
;; enforce grammar
;;
;; add arithmetic operators using CLP(FD)

