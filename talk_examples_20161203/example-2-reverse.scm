;; Example 2a: failing to synthesize all of reverse

;; Definitions
,A

;; Test Cases
(reverse '()) => '()
(reverse '(,g1)) => `(,g1)
(reverse '(,g2 ,g3)) => `(,g3 ,g2)


;; Example 2b: synthesizing reverse with reverse-acc
;; Definitions
(define reverse
  (lambda (xs)
    (reverse-acc xs '())))

(define reverse-acc
  (lambda (xs acc)
    ,A))

;; Keep Test Cases


;; Example 2c: synthesizing reverse with reduce
;; Definitions
(define reverse
  (lambda (xs)
    ,A))

(define reduce
  (lambda (f acc xs)
    (if (null? xs)
      acc
      (reduce f (f (car xs) acc) (cdr xs)))))

;; Keep Test Cases
