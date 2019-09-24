;; 'test' was originally written by Oleg Kiselyov
;;
;; 'time-test' and 'todo' are courtesy of Nada Amin

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (printf "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                     'tested-expression expected produced)))))))

(define-syntax time-test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (test title
       (time tested-expression)
       expected-result))))

(define-syntax todo
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (printf "TODO ~s\n" title))))
