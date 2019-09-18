;(load "mk.scm")
;(load "z3-driver.scm")
;(load "test-check.scm")
(load "../clpsmt-miniKanren/full-interp.scm")

(test "symbolic-execution-1a"
  (run 10 (q)
    (evalo
      `((lambda (n)
          (if (= 137 n)
              'foo
              'bar))
        ,q)
      'foo))
  '(137
    '137
    (((lambda _.0 137)) (sym _.0))
    (((lambda _.0 137) _.1) (num _.1) (sym _.0))
    (((lambda _.0 137) _.1 _.2) (num _.1 _.2) (sym _.0))
    (((lambda _.0 137) _.1 _.2 _.3) (num _.1 _.2 _.3) (sym _.0))
    (((lambda _.0 137) list) (sym _.0))
    (((lambda _.0 137) list _.1) (num _.1) (sym _.0))
    ((match _.0 (_.0 137) . _.1) (num _.0))
    (((lambda _.0 137) _.1 _.2 _.3 _.4) (num _.1 _.2 _.3 _.4) (sym _.0))))

(test "symbolic-execution-1b"
  (run* (q)
    (evalo
      `((lambda (n)
          (if (= 137 n)
              'foo
              'bar))
        ',q)
      'foo))
  '(137))

(test "symbolic-execution-2a"
  (run 10 (q)
    (evalo
      `((lambda (n)
          (if (= 137 n)
              'foo
              'bar))
        ,q)
      'bar))
  '(138 139 140 141 142 143 144 145 146 147))

(test "symbolic-execution-2b"
  (run 10 (q)
    (fresh (a d)
      (== `(,a . ,d) q))
    (evalo
      `((lambda (n)
          (if (= 137 n)
              'foo
              'bar))
        ,q)
      'bar))
  '('138 '139 '140 '141 '142 '143 '144 '145 '146 '147))

(test "symbolic-execution-2c"
  (run 10 (q)
    (evalo
      `((lambda (n)
          (if (= 137 n)
              'foo
              'bar))
        ',q)
      'bar))
  '(138 139 140 141 142 143 144 145 146 147))

(test "symbolic-execution-3a"
  (run* (q)
    (evalo
      `((lambda (n)
          (if (= (+ (* n 3) 5) 14359371734)
              'foo
              'bar))
        ',q)
      'foo))
  '(4786457243))

(test "symbolic-execution-4a"
  (run* (q)
    (evalo
      `((lambda (n)
          (if (= (+ (* n 17) 5) 81369773136)
              'foo
              'bar))
        ',q)
      'foo))
  '(4786457243))

(test "symbolic-execution-5a"
  (run* (q)
    (evalo
      `((lambda (n)
          (if (= (+ (* n 17) 5) 127142397731434)
              'foo
              'bar))
        ',q)
      'foo))
  '(7478964572437))




(load "while-abort.scm")

;;; The following example is adapted from:
;;;
;;; https://github.com/webyrd/polyconf-2015/blob/master/talk-code/while-interpreter/while-abort-tests.scm

;;; symbolic execution example from slide 7 of Stephen Chong's slides
;;; on symbolic execution (contains contents from Jeff Foster's
;;; slides)
;;;
;;; http://www.seas.harvard.edu/courses/cs252/2011sp/slides/Lec13-SymExec.pdf

;;;  1. int a = α, b = β, c = γ
;;;  2.             // symbolic
;;;  3. int x = 0, y = 0, z = 0;
;;;  4. if (a) {
;;;  5.   x = -2;
;;;  6. }
;;;  7. if (b < 5) {
;;;  8.   if (!a && c)  { y = 1; }
;;;  9.   z = 2;
;;; 10. }
;;; 11. assert(x+y+z!=3)

;;; we will model the 'assert' using 'if' and 'abort'

;;; Slightly modified version that we are actually modelling:

;;;  1. int a := α, b := β, c := γ
;;;  4. if (a != 0) {
;;;  5.   x := -2;
;;;  6. }
;;;  7. if (b < 5) {
;;;  8.   if ((a = 0) && (c != 0))  { y := 1; }
;;;  9.   z := 2;
;;; 10. }
;;; 11. if (x+(y+z) != 3) {
;;;       abort
;;;     }


(define symbolic-exec-prog
  `(seq
     (if (!= a 0)
         (:= x -2)
         (skip))
     (seq
       (if (< b 5)
           (seq
             (if (and (= a 0) (!= c 0))
                 (:= y 1)
                 (skip))
             (:= z 2))
           (skip))
       (if (!= (+ x (+ y z)) 3)
           (skip)
           (abort)))))

(test "symbolic-exec-prog-a"
  (run 8 (q)
    (fresh (alpha beta gamma s)
      (== (list alpha beta gamma s) q)
      (->o
       `(,symbolic-exec-prog
         ((a . ,alpha)
          (b . ,beta)
          (c . ,gamma)))
       `(abort ,s))))
  '((0 4 1 ((z . 2) (y . 1) (a . 0) (b . 4) (c . 1)))
    (0 0 -1 ((z . 2) (y . 1) (a . 0) (b . 0) (c . -1)))
    (0 -1 -2 ((z . 2) (y . 1) (a . 0) (b . -1) (c . -2)))
    (0 -2 -3 ((z . 2) (y . 1) (a . 0) (b . -2) (c . -3)))
    (0 -3 -4 ((z . 2) (y . 1) (a . 0) (b . -3) (c . -4)))
    (0 -4 -5 ((z . 2) (y . 1) (a . 0) (b . -4) (c . -5)))
    (0 -5 -6 ((z . 2) (y . 1) (a . 0) (b . -5) (c . -6)))
    (0 -6 -7 ((z . 2) (y . 1) (a . 0) (b . -6) (c . -7)))))

(test "symbolic-exec-prog-b"
  (run 8 (q)
    (fresh (alpha beta gamma s)
      (== (list alpha beta gamma s) q)
      (z/assert `(<= 0 ,alpha))
      (z/assert `(<= 0 ,beta))
      (z/assert `(<= 0 ,gamma))
      (->o
       `(,symbolic-exec-prog
         ((a . ,alpha)
          (b . ,beta)
          (c . ,gamma)))
       `(abort ,s))))
  '((0 0 1 ((z . 2) (y . 1) (a . 0) (b . 0) (c . 1)))
    (0 1 2 ((z . 2) (y . 1) (a . 0) (b . 1) (c . 2)))
    (0 2 3 ((z . 2) (y . 1) (a . 0) (b . 2) (c . 3)))
    (0 3 4 ((z . 2) (y . 1) (a . 0) (b . 3) (c . 4)))
    (0 4 5 ((z . 2) (y . 1) (a . 0) (b . 4) (c . 5)))
    (0 4 6 ((z . 2) (y . 1) (a . 0) (b . 4) (c . 6)))
    (0 4 7 ((z . 2) (y . 1) (a . 0) (b . 4) (c . 7)))
    (0 4 8 ((z . 2) (y . 1) (a . 0) (b . 4) (c . 8)))))

(test "symbolic-exec-prog-c"
  (run 1 (q)
    (fresh (alpha beta gamma s)
      (== (list alpha beta gamma s) q)
      (z/assert `(not (= 0 ,alpha)))
      (z/assert `(<= 0 ,beta))
      (z/assert `(<= 0 ,gamma))
      (->o
       `(,symbolic-exec-prog
         ((a . ,alpha)
          (b . ,beta)
          (c . ,gamma)))
       `(abort ,s))))
  '())

(test "symbolic-exec-prog-d"
  (run 1 (q)
    (fresh (alpha beta gamma s)
      (== (list alpha beta gamma s) q)
      (z/assert `(not (= 0 ,alpha)))
      (->o
       `(,symbolic-exec-prog
         ((a . ,alpha)
          (b . ,beta)
          (c . ,gamma)))
       `(abort ,s))))
  '())

(test "symbolic-exec-prog-e"
  (run 8 (q)
    (fresh (alpha beta gamma s)
      (== (list alpha beta gamma s) q)
      (z/assert `(not (= 0 ,beta)))
      (->o
       `(,symbolic-exec-prog
         ((a . ,alpha)
          (b . ,beta)
          (c . ,gamma)))
       `(abort ,s))))
  '((0 1 1 ((z . 2) (y . 1) (a . 0) (b . 1) (c . 1)))
    (0 -1 -1 ((z . 2) (y . 1) (a . 0) (b . -1) (c . -1)))
    (0 -2 -2 ((z . 2) (y . 1) (a . 0) (b . -2) (c . -2)))
    (0 -3 -3 ((z . 2) (y . 1) (a . 0) (b . -3) (c . -3)))
    (0 -4 -4 ((z . 2) (y . 1) (a . 0) (b . -4) (c . -4)))
    (0 -5 -5 ((z . 2) (y . 1) (a . 0) (b . -5) (c . -5)))
    (0 -6 -6 ((z . 2) (y . 1) (a . 0) (b . -6) (c . -6)))
    (0 2 -7 ((z . 2) (y . 1) (a . 0) (b . 2) (c . -7)))))
