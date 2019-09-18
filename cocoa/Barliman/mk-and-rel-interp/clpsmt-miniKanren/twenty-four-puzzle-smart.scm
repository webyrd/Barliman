(load "mk.scm")
(load "mk-streaming-interface.scm")
(load "z3-driver.scm")
(load "test-check.scm")

;;; Classic 24 math puzzle, as described at:
;;;
;;; https://www.mathsisfun.com/puzzles/24-from-8-8-3-3-solution.html
;;;
;;; and
;;;
;;; http://www.4nums.com/game/difficulties/

;;; This version of code is restricted to integer values, which means solutions like
;;;
;;; 8/(3-(8/3))
;;; = 8/(1/3)
;;; = 24
;;;
;;; do *not* work!

#|
;;; Original defn of remove-one-elemento, using (== x a) rather than (z/assert `(= ,x ,a)).
;;; Which version is preferable?
;;; What are the tradeoffs?

(define remove-one-elemento
  (lambda (x ls out)
    (fresh (a d)
      (== `(,a . ,d) ls)
      (conde
        ((== x a) (== d out))
        ((=/= x a)
         (fresh (res)
           (== `(,a . ,res) out)
           (remove-one-elemento x d res)))))))
|#

;;; optimized version, more in the spirit of 24:
;;; assumes that 'ls' is a list of integers in
;;; *non-decreasing* order.
(define remove-one-elemento
  (lambda (x ls out)
    (fresh (a d)
      (== `(,a . ,d) ls)
      (numbero a)
      (conde
        ((z/assert `(= ,a ,x))
         (== d out))
        ((z/assert `(< ,a ,x))
         (fresh (res)
           (== `(,a . ,res) out)
           (remove-one-elemento x d res)))))))

(define puzzleo
  (lambda (expr num* max-ops val num*^ max-ops^)
    (conde
      
      [(numbero expr)
       ;; Originally used (== expr val).
       ;; Which version is preferable?
       ;; What are the tradeoffs?
       (z/assert `(and (= ,expr ,val) (= ,max-ops ,max-ops^)))
       (remove-one-elemento expr num* num*^)]

      [(fresh (op e1 e2 n1 n2 num*^^ max-ops-1 max-ops^^)
         (== `(,op ,e1 ,e2) expr)
         (conde
           [(conde
              [(== '+ op)]
              [(== '* op)])
            (z/assert `(and (< 0 ,max-ops) (= (- ,max-ops 1) ,max-ops-1) (= ,val (,op ,n1 ,n2))))
            (conde
              ;; break symmetry for commutative operators
              [(numbero e1) (numbero e2)
               (z/assert `(<= ,e1 ,e2))]
              [(numbero e1)
               (fresh (o2 a2 b2)
                 (== `(,o2 ,a2 ,b2) e2))]
              [(fresh (o1 a1 b1)
                 (== `(,o1 ,a1 ,b1) e1))
               (fresh (o2 a2 b2)
                 (== `(,o2 ,a2 ,b2) e2))])]
           [(== '- op)
            (z/assert `(and (< 0 ,max-ops) (= (- ,max-ops 1) ,max-ops-1) (= ,val (,op ,n1 ,n2))))]
           [(== '/ op)
            (z/assert `(and (< 0 ,max-ops) (= (- ,max-ops 1) ,max-ops-1) (not (= ,n2 0)) (= ,val (div ,n1 ,n2))))])
         (puzzleo e1 num* max-ops-1 n1 num*^^ max-ops^^)
         (puzzleo e2 num*^^ max-ops^^ n2 num*^ max-ops^))]
      
      )))

(define puzzle-drivero
  (lambda (expr num*)
    (puzzleo expr num* 3 24 '() 0)))

(test "remove-one-elemento-a"
  (run* (q)
    (fresh (x out)
      (== (list x out) q)
      (remove-one-elemento x '(2 2 10 10) out)))
  '((2 (2 10 10))
    (10 (2 2 10))))



(test "24-puzzle-refute-a"
  (run* (e) (puzzleo e '() 0 24 '() 0))
  '())

(test "24-puzzle-refute-b"
  (run* (e) (puzzleo e '(0) 1 24 '() 0))
  '())

(test "24-puzzle-refute-c"
  (run* (e) (puzzleo e '(1) 1 24 '() 0))
  '())



(test "24-puzzle-a-check-answer-a"
  (run* (e) (== '(* 8 (+ 1 (+ 1 1))) e) (puzzle-drivero e '(1 1 1 8)))
  '((* 8 (+ 1 (+ 1 1)))))

(test "24-puzzle-a-check-answer-b"
  (run* (e) (== '(+ 8 (+ 1 (+ 1 1))) e) (puzzle-drivero e '(1 1 1 8)))
  '())


;; *** z3-counter-check-sat count: 28203
;; *** z3-counter-get-model count: 41
;; (time (test "24-puzzle-i" ...))
;;     214 collections
;;     20.592048386s elapsed cpu time, including 0.275569252s collecting
;;     718.830567000s elapsed real time, including 0.277166000s collecting
;;     1743521856 bytes allocated, including 1708956160 bytes reclaimed
(time
  (test "24-puzzle-i"
    (let ((c1 z3-counter-check-sat)
          (c2 z3-counter-get-model))
      (let ((ans (streaming-run* (e) (puzzle-drivero e '(4 6 7 7)))))
        (printf "*** z3-counter-check-sat count: ~s\n" (- z3-counter-check-sat c1))
        (printf "*** z3-counter-get-model count: ~s\n" (- z3-counter-get-model c2))
        ans))    
    '((- 7 (- 7 (* 4 6)))
      (+ 4 (+ 6 (+ 7 7)))
      (* 4 (- 6 (- 7 7)))
      (+ 4 (+ 7 (+ 6 7)))
      (* 4 (/ 6 (/ 7 7)))
      (+ 6 (+ 4 (+ 7 7)))
      (* 4 (- 7 (- 7 6)))
      (+ 6 (+ 7 (+ 4 7)))
      (* 4 (- 7 (/ 7 6)))
      (* 6 (- 4 (- 7 7)))
      (* 4 (+ 6 (- 7 7)))
      (* 4 (+ 7 (- 6 7)))
      (* 4 (- (+ 6 7) 7))
      (* 4 (* 6 (/ 7 7)))
      (* 6 (/ 4 (/ 7 7)))
      (* 4 (/ (* 6 7) 7))
      (+ 7 (+ 4 (+ 6 7)))
      (* 6 (- 7 (- 7 4)))
      (+ 7 (+ 6 (+ 4 7)))
      (+ 7 (+ 7 (+ 4 6)))
      (* 6 (+ 4 (- 7 7)))
      (+ 7 (- (* 4 6) 7))
      (* 6 (+ 7 (- 4 7)))
      (* 6 (- (+ 4 7) 7))
      (* 6 (* 4 (/ 7 7)))
      (* 6 (/ (* 4 7) 7))
      (- (* 4 6) (- 7 7))
      (/ (* 4 6) (/ 7 7))
      (+ (+ 4 6) (+ 7 7))
      (+ (+ 4 7) (+ 6 7))
      (/ (* 7 7) (- 6 4))
      (+ (- 7 7) (* 4 6))
      (- (+ 7 (* 4 6)) 7)
      (/ (* 4 (* 6 7)) 7)
      (* (* 4 6) (/ 7 7))
      (+ (+ 6 7) (+ 4 7))
      (+ (* 4 6) (- 7 7))
      (/ (* 6 (* 4 7)) 7)
      (* (/ 7 7) (* 4 6))
      (/ (* 7 (* 4 6)) 7)
      (+ (+ 7 7) (+ 4 6)))))

(time
  (test "24-puzzle-j"
    (streaming-run* (e) (puzzle-drivero e '(1 2 5 10)))
    '((- 5 (- 1 (* 2 10)))
      (+ 5 (- (* 2 10) 1))
      (+ (- 5 1) (* 2 10))
      (- (* 2 10) (- 1 5))
      (- (+ 5 (* 2 10)) 1)
      (- (* 5 (/ 10 2)) 1)
      (- (/ (* 5 10) 2) 1)
      (/ (- (* 5 10) 1) 2)
      (+ (* 2 10) (- 5 1)))))

(time
  (test "24-puzzle-k"
    (streaming-run* (e) (puzzle-drivero e '(3 7 8 9)))
    '((* 3 (- 7 (- 8 9)))
      (* 3 (- 8 (/ 7 9)))
      (* 3 (- 9 (- 8 7)))
      (* 3 (- 9 (/ 8 7)))
      (* 3 (/ 8 (/ 9 7)))
      (* 3 (+ 7 (- 9 8)))
      (* 3 (+ 7 (/ 9 8)))
      (* 3 (+ 8 (/ 7 9)))
      (* 3 (+ 9 (- 7 8)))
      (* 3 (- (+ 7 9) 8))
      (* 3 (* 8 (/ 9 7)))
      (* 8 (- 3 (/ 7 9)))
      (* 8 (/ 3 (/ 9 7)))
      (* 8 (+ 3 (/ 7 9)))
      (* 8 (* 3 (/ 9 7)))
      (* 8 (/ (* 3 9) 7))
      (- (* 3 8) (/ 7 9))
      (/ (* 3 8) (/ 9 7))
      (+ (/ 7 9) (* 3 8))
      (* (* 3 8) (/ 9 7))
      (+ (* 3 8) (/ 7 9))
      (* (/ 9 7) (* 3 8)))))

(time
  (test "24-puzzle-a-all-streaming"
    (streaming-run* (e) (puzzle-drivero e '(1 1 1 8)))
    '((* 8 (+ 1 (+ 1 1))))))

(time
  (test "24-puzzle-g-all-streaming"
    (streaming-run* (e) (puzzle-drivero e '(2 2 10 10)))
    '((+ 2 (+ 2 (+ 10 10)))
      (+ 2 (+ 10 (+ 2 10)))
      (+ 10 (+ 2 (+ 2 10)))
      (+ 10 (+ 10 (+ 2 2)))
      (+ 10 (+ 10 (* 2 2)))
      (+ (+ 2 2) (+ 10 10))
      (+ (+ 2 10) (+ 2 10))
      (+ (* 2 2) (+ 10 10))
      (+ (+ 10 10) (+ 2 2))
      (+ (+ 10 10) (* 2 2)))))

(time
  (test "24-puzzle-h-all-streaming"
    (streaming-run* (e) (puzzle-drivero e '(2 2 2 12)))
    '((- 2 (- 2 (* 2 12)))
      (* 2 (- 2 (- 2 12)))
      (* 2 (- 12 (- 2 2)))
      (* 2 (/ 12 (/ 2 2)))
      (+ 2 (- (* 2 12) 2))
      (* 2 (+ 2 (- 12 2)))
      (* 2 (- (+ 2 12) 2))
      (* 2 (* 2 (/ 12 2)))
      (* 12 (- 2 (- 2 2)))
      (* 2 (+ 12 (- 2 2)))
      (* 2 (* 12 (/ 2 2)))
      (* 2 (/ (* 2 12) 2))
      (* 12 (/ 2 (/ 2 2)))
      (* 12 (+ 2 (- 2 2)))
      (* 12 (- (+ 2 2) 2))
      (+ (- 2 2) (* 2 12))
      (* 12 (* 2 (/ 2 2)))
      (* 12 (- (* 2 2) 2))
      (* 12 (/ (+ 2 2) 2))
      (* 12 (/ (* 2 2) 2))
      (* (/ 2 2) (* 2 12))
      (- (* 2 12) (- 2 2))
      (/ (* 2 12) (/ 2 2))
      (* (+ 2 2) (/ 12 2))
      (- (+ 2 (* 2 12)) 2)
      (* (* 2 2) (/ 12 2))
      (/ (* 2 (* 2 12)) 2)
      (* (* 2 12) (/ 2 2))
      (+ (* 2 12) (- 2 2))
      (* (/ 12 2) (+ 2 2))
      (* (/ 12 2) (* 2 2))
      (/ (* 12 (+ 2 2)) 2)
      (/ (* 12 (* 2 2)) 2))))
