(load "mk.scm")
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

      [(fresh (a1 a2 n1 n2 num*^^ max-ops-1 max-ops^^)
         (== `(+ ,a1 ,a2) expr)
         (z/assert `(and (= ,val (+ ,n1 ,n2)) (< 0 ,max-ops) (= (- ,max-ops 1) ,max-ops-1)))
         (puzzleo a1 num* max-ops-1 n1 num*^^ max-ops^^)
         (puzzleo a2 num*^^ max-ops^^ n2 num*^ max-ops^))]

      [(fresh (a1 a2 n1 n2 num*^^ max-ops-1 max-ops^^)
         (== `(- ,a1 ,a2) expr)
         (z/assert `(and (= ,val (- ,n1 ,n2)) (< 0 ,max-ops) (= (- ,max-ops 1) ,max-ops-1)))
         (puzzleo a1 num* max-ops-1 n1 num*^^ max-ops^^)
         (puzzleo a2 num*^^ max-ops^^ n2 num*^ max-ops^))]

      [(fresh (a1 a2 n1 n2 num*^^ max-ops-1 max-ops^^)
         (== `(* ,a1 ,a2) expr)
         (z/assert `(and (= ,val (* ,n1 ,n2)) (< 0 ,max-ops) (= (- ,max-ops 1) ,max-ops-1)))
         (puzzleo a1 num* max-ops-1 n1 num*^^ max-ops^^)
         (puzzleo a2 num*^^ max-ops^^ n2 num*^ max-ops^))]

      [(fresh (a1 a2 n1 n2 num*^^ max-ops-1 max-ops^^)
         (== `(/ ,a1 ,a2) expr)
         (z/assert `(and (< 0 ,max-ops) (= (- ,max-ops 1) ,max-ops-1) (not (= ,n2 0)) (= ,val (div ,n1 ,n2))))
         (puzzleo a1 num* max-ops-1 n1 num*^^ max-ops^^)
         (puzzleo a2 num*^^ max-ops^^ n2 num*^ max-ops^))]
      
      )))

(test "remove-one-elemento-a"
  (run* (q)
    (fresh (x out)
      (== (list x out) q)
      (remove-one-elemento x '(2 2 10 10) out)))
  '((2 (2 10 10))
    (10 (2 2 10))))

;; Keep track of minimum number of numbers that can be used in the search!
;; This query should terminate!
#|
(test "24-puzzle-refute-a"
  (run 1 (e) (puzzleo e '() 3 24 '() 0))
  '())

(test "24-puzzle-refute-b"
  (run 1 (e) (puzzleo e '(0) 3 24 '() 0))
  '())

(test "24-puzzle-refute-c"
  (run 1 (e) (puzzleo e '(1) 3 24 '() 0))
  '())
|#

(test "24-puzzle-a-check-answer-a"
  (run 1 (e) (== '(* 8 (+ 1 (+ 1 1))) e) (puzzleo e '(1 1 1 8) 3 24 '() 0))
  '((* 8 (+ 1 (+ 1 1)))))

(test "24-puzzle-a-check-answer-b"
  (run 1 (e) (== '(+ 8 (+ 1 (+ 1 1))) e) (puzzleo e '(1 1 1 8) 3 24 '() 0))
  '())


;; On Will's lappy--timings, according to Chez Scheme:
;; vast majority of the time spent in Z3.
;;
;; run 6: 3.2s CPU time, 103s real time
(test "24-puzzle-h"
  (run 6 (e) (puzzleo e '(2 2 2 12) 3 24 '() 0))
  '((+ (- 2 2) (* 2 12))
    (+ (- 2 2) (* 12 2))
    (/ (* 2 12) (/ 2 2))
    (+ 2 (- (* 2 12) 2))
    (+ 2 (- (* 12 2) 2))
    (* 2 (+ 2 (- 12 2)))))

#!eof


;; On Will's lappy--timings, according to Chez Scheme:
;; vast majority of the time spent in Z3.
;;
;; run 1: 2.6s CPU time, 85s real time
;; ((* 8 (+ 1 (+ 1 1))))
;;
;; run 2: 3.5s CPU time, 118s real time
;; ((* 8 (+ 1 (+ 1 1)))
;;  (* 8 (+ (+ 1 1) 1)))
;;
;; run 3: 9.4s CPU, 312s real time
;; ((* 8 (+ 1 (+ 1 1)))
;;  (* 8 (+ (+ 1 1) 1))
;;  (* (+ 1 (+ 1 1)) 8))
(test "24-puzzle-a"
  (run 1 (e) (puzzleo e '(1 1 1 8) 3 24 '() 0))
  '((* 8 (+ 1 (+ 1 1)))))

(test "24-puzzle-g"
  (run 1 (e) (puzzleo e '(2 2 10 10) 3 24 '() 0))
  '((+ 2 (+ 2 (+ 10 10)))))

#!eof

(test "24-puzzle-i"
  (run 1 (e) (puzzleo e '(4 6 7 7) 24 '()))
  '((+ 4 (+ 6 (+ 7 7)))))

#!eof

(test "24-puzzle-j"
  (run 1 (e) (puzzleo e '(1 2 5 10) 24 '()))
  '?)

;;; boring!!
(test "24-puzzle-b"
  (run 1 (q)
    (fresh (e num* n1 n2 n3 n4)
      (== (list e num*) q)
      (== `(,n1 ,n2 ,n3 ,n4) num*)
      (puzzleo e num* 24 '())))
  '(((+ 24 (+ 0 (+ 0 0))) (24 0 0 0))))

(test "24-puzzle-c"
  (run 20 (e)
    (fresh (num* n1 n2 n3 n4)
      (z/assert `(< 1 ,n1))
      (z/assert `(< 1 ,n2))
      (z/assert `(< 1 ,n3))
      (z/assert `(< 1 ,n4))
      (== `(,n1 ,n2 ,n3 ,n4) num*)
      (puzzleo e num* 24 '())))
  '((+ 18 (+ 2 (+ 2 2)))
    (+ 15 (+ 3 (+ 3 3)))
    (+ 12 (+ 4 (+ 4 4)))
    (+ 9 (+ 5 (+ 5 5)))
    (+ 13 (+ 4 (+ 4 3)))
    (+ 11 (+ 6 (+ 4 3)))
    (+ 10 (+ 7 (+ 4 3)))
    (+ 12 (+ 4 (+ 5 3)))
    (+ 11 (+ 4 (+ 6 3)))
    (+ 10 (+ 6 (+ 5 3)))
    (+ 9 (+ 8 (+ 4 3)))
    (+ 8 (+ 9 (+ 4 3)))
    (+ 14 (+ 4 (+ 3 3)))
    (+ 10 (+ 5 (+ 6 3)))
    (+ 13 (+ 5 (+ 3 3)))
    (+ 12 (+ 5 (+ 4 3)))
    (+ 10 (+ 4 (+ 7 3)))
    (+ 10 (+ 8 (+ 3 3)))
    (+ 9 (+ 9 (+ 3 3)))
    (+ 7 (+ 10 (+ 4 3)))))

(test "24-puzzle-d"
  (run 10 (e)
    (fresh (num* n1 n2 n3 n4 op1 op2 op3 e1 e2 e3 e4)
      (z/assert `(< 0 ,n1))
      (z/assert `(< 0 ,n2))
      (z/assert `(< 0 ,n3))
      (z/assert `(< 0 ,n4))
      (== `(,n1 ,n2 ,n3 ,n4) num*)
      (== `(,op1 (,op2 ,e1 ,e2) (,op3 ,e3 ,e4)) e)
      (puzzleo e num* 24 '())))
  '((+ (+ 21 1) (+ 1 1))
    (+ (+ 18 2) (+ 2 2))
    (+ (+ 15 3) (+ 3 3))
    (+ (+ 12 4) (+ 4 4))
    (+ (+ 16 3) (+ 3 2))
    (+ (+ 14 5) (+ 3 2))
    (+ (+ 13 6) (+ 3 2))
    (+ (+ 15 3) (+ 4 2))
    (+ (+ 14 3) (+ 5 2))
    (+ (+ 13 5) (+ 4 2))))

(test "24-puzzle-e"
  (run 10 (e)
    (fresh (num* n1 n2 n3 n4 op e1 e2)
      (z/assert `(< 0 ,n1))
      (z/assert `(< 0 ,n2))
      (z/assert `(< 0 ,n3))
      (z/assert `(< 0 ,n4))
      (== `(,n1 ,n2 ,n3 ,n4) num*)
      (=/= op '+)
      (== `(,op ,e1 ,e2) e)
      (puzzleo e num* 24 '())))
  '((- 27 (+ 1 (+ 1 1)))
    (- 30 (+ 2 (+ 2 2)))
    (- 33 (+ 3 (+ 3 3)))
    (- 36 (+ 4 (+ 4 4)))
    (- 32 (+ 3 (+ 3 2)))
    (- 34 (+ 5 (+ 3 2)))
    (- 35 (+ 6 (+ 3 2)))
    (- 33 (+ 3 (+ 4 2)))
    (- 34 (+ 3 (+ 5 2)))
    (- 35 (+ 5 (+ 4 2)))))

(test "24-puzzle-f"
  (run 10 (e)
    (fresh (num* n1 n2 n3 n4 op e1 e2)
      (z/assert `(< 0 ,n1))
      (z/assert `(< 0 ,n2))
      (z/assert `(< 0 ,n3))
      (z/assert `(< 0 ,n4))
      (== `(,n1 ,n2 ,n3 ,n4) num*)
      (== op '*)
      (== `(,op ,e1 ,e2) e)
      (puzzleo e num* 24 '())))
  '((* 1 (+ 1 (+ 1 22)))
    (* 8 (+ 1 (+ 1 1)))
    (* 6 (+ 2 (+ 1 1)))
    (* 4 (+ 4 (+ 1 1)))
    (* 3 (+ 6 (+ 1 1)))
    (* 2 (+ 10 (+ 1 1)))
    (* 1 (+ 22 (+ 1 1)))
    (* 6 (+ 1 (+ 1 2)))
    (* 4 (+ 3 (+ 1 2)))
    (* 3 (+ 5 (+ 1 2)))))
