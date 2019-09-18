(load "mk.scm")
(load "z3-driver.scm")
(load "test-check.scm")
(load "while-abort.scm")

;;; Adapted from https://github.com/webyrd/polyconf-2015/blob/master/talk-code/while-interpreter/while-abort-tests.scm

(test "abort-1"
  (run 1 (q)
    (->o
      `((abort)
        ())
      q))
  '((abort ())))

(test "abort-2"
  (run 1 (q)
    (->o
      `((seq
         (:= y 4)
         (seq
          (abort)
          (:= x 5)))
        ())
      q))
  '((abort ((y . 4)))))




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
  (run 4 (q)
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
    (0 -2 -3 ((z . 2) (y . 1) (a . 0) (b . -2) (c . -3)))))

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





;;; Slightly modified version that we are actually modelling:

;;;  1. int a = α, b = β, c = γ
;;;  2.             // symbolic
;;;  3. int x = 0, y = 0, z = 0;
;;;  4. if (a) {
;;;  5.   x = 5;
;;;  6. }
;;;  7. if (b <= 4) {
;;;  8.   if (!a && c)  { y = 1; }
;;;  9.   z = 2;
;;; 10. }
;;; 11. assert(x+y+z!=3)

;;;  1. int a := α, b := β, c := γ
;;;  4. if !(a = 0) {
;;;  5.   x := 5;
;;;  6. }
;;;  7. if (b <= 4) {
;;;  8.   if ((a = 0) && !(c = 0))  { y := 1; }
;;;  9.   z := 2;
;;; 10. }
;;; 11. if !(x+(y+z) = 3) {
;;;       abort
;;;     }


(define symbolic-exec-prog-1
  `(seq
     (if (not (= 0 a))
         (:= x 5) ;; lol negative numbers!
         (skip))
     (seq
       (if (<= b 4) ;; might want to use numbero to automatically convert numbers to Oleg form
           (seq
             (if (and (= 0 a)
                      (not
                        (= 0 c)))
                 (:= y 1)
                 (skip))
             (:= z 2))
           (skip))
       (if (= (+ x (+ y z)) 3)
           (abort)
           (skip)))))

(test "symbolic-exec-prog-1a"
  (run 4 (q)
    (fresh (alpha beta gamma s)
      (== (list alpha beta gamma s) q)
      (->o
       `(,symbolic-exec-prog-1
         ((a . ,alpha)
          (b . ,beta)
          (c . ,gamma)))
       `(abort ,s))))
  '((0 4 1
       ((z . 2)
        (y . 1)
        (a . 0)
        (b . 4)
        (c . 1)))
    (0 0 -1
       ((z . 2)
        (y . 1)
        (a . 0)
        (b . 0)
        (c . -1)))
    (0 -1 2
       ((z . 2)
        (y . 1)
        (a . 0)
        (b . -1)
        (c . 2)))
    (0 -2 -2
       ((z . 2)
        (y . 1)
        (a . 0)
        (b . -2)
        (c . -2)))))

;; simplified answer format
(test "symbolic-exec-prog-1b"
  (run 4 (alpha beta gamma)
    (fresh (s)
      (->o
       `(,symbolic-exec-prog-1
         ((a . (num ,alpha))
          (b . (num ,beta))
          (c . (num ,gamma))))
       `(abort ,s))))
  '(((()      ; a == 0
      (0 0 1) ; b == 4
      _.0)    ; c != 0
     (=/= ((_.0 ())))
     (absento (abort _.0)))
    ((()      ; a == 0
      ()      ; b == 0
      _.0)    ; c != 0
     (=/= ((_.0 ())))
     (absento (abort _.0)))
    ((()      ; a == 0
      (1)     ; b == 1
      _.0)    ; c != 0
     (=/= ((_.0 ())))
     (absento (abort _.0)))
    ((()       ; a == 0
      (_.0 1)  ; b == 2 or 3
      _.1)     ; c != 3
     (=/= ((_.1 ())))
     (absento (abort _.0) (abort _.1)))))

;; very simplified answer format
(test "symbolic-exec-prog-1c"
  (run 1 (alpha beta gamma)
    (fresh (s)
      (->o
       `(,symbolic-exec-prog-1
         ((a . (num ,alpha))
          (b . (num ,beta))
          (c . (num ,gamma))))
       `(abort ,s))))
  '(((()      ; a == 0
      (0 0 1) ; b == 4
      _.0)    ; c != 0
     (=/= ((_.0 ())))
     (absento (abort _.0)))))

(test "symbolic-exec-prog-1-subexpr-1"
  (run* (c-val val)
    (Bo
     `(and (= ,(num 0) a)
            (not
             (= ,(num 0) c)))
     `((a . (num ())) (c . (num ,c-val)))
     val))
  '(((_.0 tt) (=/= ((_.0 ()))))
    (() ff)))


#|

;;; Example 1 from Zvonimir Rakamaric's CS 6110 slides on symbolic
;;; testing:
;;;
;;; http://www.zvonimir.info/teaching/cs6110-2015-spring/cs6110_lecture_10.pdf
;;
;; int x, y;
;; if (x > y) {
;;   x = x + y;
;;   y = x – y;
;;   x = x – y;
;;   if (x > y)
;;     assert false;
;; }

;;; Is this example broken?  Supposedly x = 4, y = 3 works, but this
;;; doesn't seem to be the case.  Is the assert even reachable?

(define symbolic-exec-prog-2
  `(seq
     (if (< y x)
         (seq
           (:= x (+ x y))
           (seq
             (:= y (- x y))
             (seq
              (:= x (- x y))
              (if (< y x)
                  (abort)
                  (skip)))))
         (skip))
     (skip)))

(test "symbolic-exec-prog-2b"
  (run 1 (x y)
    (== (build-num 4) x)
    (== (build-num 3) y)
    (fresh (s)
      (->o
       `(,symbolic-exec-prog-2
         ((x . (num ,x))
          (y . (num ,y))))
       s)))
  '???)


(test "symbolic-exec-prog-2a"
  (run 1 (alpha beta)
    (fresh (s)
      (->o
       `(,symbolic-exec-prog-2
         ((x . (num ,alpha))
          (y . (num ,beta))))
       `(abort ,s))))
  '(((()      ; a == 0
      (0 0 1) ; b == 4
      _.0)    ; c != 0
     (=/= ((_.0 ())))
     (absento (abort _.0)))))

|#
