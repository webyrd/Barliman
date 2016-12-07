;; Example 5: quine-ception!

;; Definitions

;; HACK: We visualize the answer to our query by synthesizing the dead code Q.
(define answer
  (lambda ()
    '(Q: ,Q)))

;; Tower of interpretation!
;;; Scheme
;;;; miniKanren
;;;;; Scheme interpreter written in miniKanren
;;;;;; Scheme interpreter (eval) relationally interpreted
;;;;;;; Scheme program (,Q) being interpreted by eval  <-- YOU ARE HERE

(define eval
  (lambda (term env)
    (match term

      ;; Construct literal data.
      (`(quote ,datum) datum)

      ;; Create a single-argument closure.
      (`(lambda (,(? symbol? x)) ,body)
        (lambda (a)
          (eval body (lambda (y)
                       (if (equal? x y) a (env y))))))

      ;; Reference a variable.
      ((? symbol? x) (env x))

      ;; Build a pair from ,a and ,b.
      (`(cons ,a ,b) (cons (eval a env) (eval b env)))

      ;; Apply ,rator to ,rand.
      (`(,rator ,rand)
        ((eval rator env) (eval rand env))))))

;; Test Cases
(eval ',Q 'initial-env) => Q

;; This takes about a minute to synthesize due to evaluating through multiple
;; nested levels of interpretive overhead.  Note: synthesizing a quine with a
;; comparable interpreter implemented directly in miniKanren is much faster.
