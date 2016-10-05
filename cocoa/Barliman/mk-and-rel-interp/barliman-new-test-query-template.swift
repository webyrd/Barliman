;; adapted from http://www.scheme.com/tspl4/exceptions.html
(define (try thunk error-symbol)
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda (x)
          (if (error? x)
              (k error-symbol)
              (raise x)))
        thunk))))

(define (final-value)
  (try
    (lambda ()
      (load simple-query-for-mondo-file-path)
      (let ((vs (query-val-simple)))
        (if (eqv? 'parse-error vs)
            'parse-error-in-defn
            (try
              (lambda ()
                (load actual-query-file-path)
                (let ((vt (test-query-fn)))
                  (if (eqv? 'parse-error vt)
                      'parse-error-in-test/answer
                    vt)))
              'illegal-sexp-in-test/answer))))
    'illegal-sexp-in-defn))

(write (final-value))
