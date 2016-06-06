; new version of matche
; fixes depth related issues, and works with dots
;
; https://github.com/calvis/cKanren/blob/dev/cKanren/matche.rkt#L54

; Note that this definition is available at syntax phase in chez and vicare due to implicit
; phasing, but not in Racket (which uses explicit phasing). Racket already has a version available
; by default though, so that's fine. This definition isn't just isn't used in Racket.
(define syntax->list
  (lambda (e)
    (syntax-case e ()
      [() '()]
      [(x . r) (cons #'x (syntax->list #'r))])))

(define-syntax defmatche
  (lambda (stx)
    (syntax-case stx ()
      [(defmatche (name args ...) clause ...)
       #'(define (name args ...)
           (matche (args ...) clause ...))])))

(define-syntax lambdae
  (syntax-rules ()
    ((_ (x ...) c c* ...)
     (lambda (x ...) (matche (x ...) c c* ...)))))

(define-syntax matche
  (lambda (stx)    
    (syntax-case stx ()
      [(matche (v ...) ([pat ...] g ...) ...)
       (let ()
         (define remove-duplicates
           (lambda (ls eq-pred)
             (cond
               [(null? ls) '()]
               [(memp (lambda (x) (eq-pred (car ls) x)) (cdr ls))
                (remove-duplicates (cdr ls) eq-pred)]
               [else (cons (car ls) (remove-duplicates (cdr ls) eq-pred))])))
         (define parse-pattern
           (lambda (args pat)
             (syntax-case #`(#,args #,pat) ()
               [(() ()) #'(() () ())]
               [((a args ...) [p pat ...])
                (with-syntax ([(p^ (c ...) (x ...))
                               (parse-patterns-for-arg #'a #'p)])
                  (with-syntax ([([pat^ ...] (c^ ...) (x^ ...))
                                 (parse-pattern #'(args ...) #'[pat ...])])
                    #'([p^ pat^ ...] (c ... c^ ...) (x ... x^ ...))))]
               [x (error 'parse-pattern "bad syntax ~s ~s" args pat)])))
         (define parse-patterns-for-arg
           (lambda (v pat)
             (define loop
               (lambda (pat)
                 (syntax-case pat (unquote ?? ?) ; ?? is the new _, since _ isn't legal in R6
                   [(unquote ??)
                    (with-syntax ([_new (generate-temporary #'?_)])
                      #'((unquote _new) () (_new)))]
                   [(unquote x)
                    (when (free-identifier=? #'x v)
                      (error 'matche "argument ~s appears in pattern at an invalid depth"
                             (syntax->datum #'x)))
                    #'((unquote x) () (x))]
                   [(unquote (? c x))
                    (when (free-identifier=? #'x v)
                      (error 'matche "argument ~s appears in pattern at an invalid depth"
                             (syntax->datum #'x)))
                    #'((unquote x) ((c x)) (x))]
                   [(a . d)
                    (with-syntax ([((pat1 (c1 ...) (x1 ...))
                                    (pat2 (c2 ...) (x2 ...)))
                                   (map loop (syntax->list #'(a d)))])
                      #'((pat1 . pat2) (c1 ... c2 ...) (x1 ... x2 ...)))]
                   [x #'(x () ())])))
             (syntax-case pat (unquote ?)
               [(unquote u)
                (cond
                  [(and (identifier? #'u)
                        (free-identifier=? v #'u))
                   #'((unquote u) () ())]
                  [else (loop pat)])]
               [(unquote (? c u))
                (cond
                  [(and (identifier? #'u)
                        (free-identifier=? v #'u))
                   #'((unquote u) ((c x)) ())]
                  [else (loop pat)])]
               [else (loop pat)])))
         (unless
             (andmap (lambda (y) (= (length (syntax->datum #'(v ...))) (length y)))
                     (syntax->datum #'([pat ...] ...)))
           (error 'matche "pattern wrong length blah"))
         (with-syntax ([(([pat^ ...] (c ...) (x ...)) ...)
                        (map (lambda (y) (parse-pattern #'(v ...) y))
                             (syntax->list #'([pat ...] ...)))])
           (with-syntax ([((x^ ...) ...)
                          (map (lambda (ls)
                                 (remove-duplicates (syntax->list ls) free-identifier=?))
                               (syntax->list #'((x ...) ...)))])
             (with-syntax ([body
                            #'(conde
                                [(fresh (x^ ...) c ... (== `[pat^ ...] ls) g ...)]
                                ...)])
               #'(let ([ls (list v ...)]) body)))))]
      [(matche v (pat g ...) ...)
       #'(matche (v) ([pat] g ...) ...)])))
