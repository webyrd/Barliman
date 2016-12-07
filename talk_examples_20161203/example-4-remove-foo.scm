;; Example 4: Refuting an incomplete remove-foo

;; Definitions
(define remove-foo
  (lambda (xs)
    (cond
      ((null? xs) '())
      ((equal? 'foo (car xs)) (cons ,B ,C))
      . ,A)))

;; Test Cases
(remove-foo '()) => '()
(remove-foo '(foo)) => '()
(remove-foo '(,g1)) => `(,g1)
(remove-foo '(foo ,g2)) => `(,g2)
(remove-foo '(,g3 foo)) => `(,g3)
