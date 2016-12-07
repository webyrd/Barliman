;; Example 3: synthesizing a transformation to map over a list

;; Definitions
(define map
  (lambda (f xs)
    (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs))))))

;; HACK: We visualize the answer to our query by synthesizing the dead code Q.
(define answer
  (lambda ()
    '(Q: (lambda . ,Q))))

;; Test Cases
(map (lambda . ,Q) '((a (b c) d) (1 (2 3) 4))) => '(b 2)
