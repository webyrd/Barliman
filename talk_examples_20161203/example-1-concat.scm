;; Example 1a: synthesizing all of concat

;; Definitions
,A

;; Test Cases
(concat '() '()) => '()
;(concat '(1) '(2)) => '(1 2)  ;; leads to overfitting
(concat '(,g1) '(,g2)) => `(,g1 ,g2)
(concat '(,g3 ,g4) '(,g5 ,g6)) => `(,g3 ,g4 ,g5 ,g6)


;; Example 1b: synthesizing concat from reduce-right

;; Definitions
(define reduce-right
  (lambda (f acc xs)
    (if (null? xs)
      acc
      (f (car xs) (reduce-right f acc (cdr xs))))))

(define concat
  (lambda (xs ys)
    ,A))

;; Keep Test Cases
