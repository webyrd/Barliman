#lang racket

(require racket/system)

(define rows
  (for/list ([i (in-range 1 10)])
    (for/list ([j (in-range 1 10)])
      (cons i j))))
(define cols
  (for/list ([i (in-range 1 10)])
    (for/list ([j (in-range 1 10)])
      (cons j i))))
(define squares
  (for*/list ([m (in-range 3)] [n (in-range 3)])
    (for*/list ([i (in-range 3)] [j (in-range 3)])
      (cons (+ i (* n 3) 1) (+ j (* m 3) 1)))))

(define (name ij)
  (let ((i (car ij)) (j (cdr ij)))
    (string->symbol (string-append "x" (string-append (number->string i) (number->string j))))))
(define (model-assoc m)
  (map (lambda (x) (match x [(list _ lhs _ _ rhs) (cons lhs rhs)])) (cdr m)))
(define (model-get m ij) (cdr (assoc (name ij) m)))
(define (model->puzzle m)
  (for/vector ([i (in-range 1 10)])
    (for/vector ([j (in-range 1 10)])
      (model-get m (cons i j)))))
(define (print-puzzle puzzle)
  (printf "~a\n" "'#(")
  (for ([line puzzle])
    (printf "   ~a\n" line))
  (printf "  ~a\n" ")"))
(define (puzzle-index puzzle i j)
  (vector-ref (vector-ref puzzle (- i 1)) (- j 1)))

(define puzzle
  '#(
     #(4 0 0 0 0 0 8 0 5)
     #(0 3 0 0 0 0 0 0 0)
     #(0 0 0 7 0 0 0 0 0)
     #(0 2 0 0 0 0 0 6 0)
     #(0 0 0 0 8 0 4 0 0)
     #(0 0 0 0 1 0 0 0 0)
     #(0 0 0 6 0 3 0 7 0)
     #(5 0 0 2 0 0 0 0 0)
     #(1 0 4 0 0 0 0 0 0)
    ))

(define (say msg)
  (printf "~a\n" msg))

(define (board-constraints say)
  (for ([i (in-range 1 10)])
    (for ([j (in-range 1 10)])
      (let ((x (name (cons i j))))
        (say `(declare-const ,x Int))
        (say `(assert (<= 1 ,x)))
        (say `(assert (<= ,x 9))))))
  (for ([c (list rows cols squares)])
    (for ([r c])
      (say `(assert (distinct . ,(map name r)))))))

(define (puzzle-constraints puzzle say)
  (for ([i (in-range 1 10)])
    (for ([j (in-range 1 10)])
      (let ((r (puzzle-index puzzle i j)))
        (when (not (= r 0))
          (let ((x (name (cons i j))))
            (say `(assert (= ,x ,r)))))))))

(define (uniqueness-constraints puzzle solution say)
  (say
   `(assert (or .
                ,(for*/list ([i (in-range 1 10)] [j (in-range 1 10)]
                             #:when (= 0 (puzzle-index puzzle i j)))
                   (let ((x (name (cons i j))))
                     `(not (= ,x ,(puzzle-index solution i j)))))))))

(define (solve-sudoku puzzle)
  (match (process "z3 -in")
    [(list p-in p-out _ p-err p-fun)
     (define (say msg)
       ;(printf "~a\n" msg)
       (fprintf p-out "~a\n" msg))
     (define (check-sat)
       (say '(check-sat))
       (flush-output p-out)
       (let ((r (read p-in)))
         (println r)
         (eq? r 'sat)))
     (define (get-model)
       (say '(get-model))
       (flush-output p-out)
       (model-assoc (read p-in)))

     (board-constraints say)
     (puzzle-constraints puzzle say)
     (if (check-sat)
         (let ((solution (model->puzzle (get-model))))
           (print-puzzle solution)

           (uniqueness-constraints puzzle solution say)

           (if (check-sat)
               (printf "~a\n" ";; solution is not unique!")
               (printf "~a\n" ";; solution is unique :)"))
           )
         (printf "~a\n" ";; no solution :("))

     (close-input-port p-in)
     (close-output-port p-out)
     (close-input-port p-err)]))

(solve-sudoku puzzle)

