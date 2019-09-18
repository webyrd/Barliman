;(load "mk.scm")
;(load "z3-driver.scm")
(load "../clpsmt-miniKanren/sign-domain.scm")
;(load "test-check.scm")
(load "../clpsmt-miniKanren/full-abstract-interp-extended.scm")

;;; WEB stopping point:  21 April 02018
;;;
;;; Not sure the arithmetic is working correctly in the abstract evaluator.
;;;
;;; Fix broken tests, including after #!eof
;;;
;;;
;;; Fix quine-generating query: (run 3 (q) (evalo q q))
;;;
;;; Exception in call-z3: error in z3 out.smt > out.txt
;;; Type (debug) to enter the debugger.
;;;
;;; (error "line 4 column 28: Sort mismatch at argument #1 for function (declare-fun > (Int Int) Bool) supplied sort is (_ BitVec 3)")
;;; sat
;;;
;;;
;;; Need to add tabling
;;;
;;; Want to get more precise answers from factorial
;;;
;;; Does this mixture of abstract value/powerset for integers, and concrete values for all other values, make sense?  Or should we use power sets for everything?  Currently the code feels a bit like concolic execution.

(test "evalo-simple-let-a"
  (run* (q)
    (evalo '(let ((foo (+ 1 2))) (* foo foo)) q))
  '((num bitvec-100)))

(test "evalo-1a"
  (run* (q)
    (evalo '(+ 1 2) q))
  '((num bitvec-100)))

(test "evalo-1b"
  (run* (q)
    (evalo '(+ 1 -2) q))
  '((num bitvec-111)))

(test "evalo-1c"
  (run* (q)
    (evalo '(* 0 -2) q))
  '((num bitvec-010)))

(test "evalo-sub1-a"
  (run* (q)
    (evalo '(sub1 5) q))
  '((num bitvec-110)))

(test "evalo-sub1-b"
  (run* (q)
    (evalo '(sub1 0) q))
  '((num bitvec-001)))

(test "evalo-sub1-c"
  (run* (q)
    (evalo '(sub1 -2) q))
  '((num bitvec-001)))

(test "evalo-sub1-d"
  (run* (q)
    (evalo '(sub1 (+ 3 4)) q))
  '((num bitvec-110)))

(test "evalo-sub1-e"
  (run* (q)
    (evalo '(sub1 (+ 3 -4)) q))
  '((num bitvec-111)))

(test "evalo-if-zero?-sub1-a"
  (run* (q)
    (evalo '(let ((x 3))
              (if (zero? x)
                  x
                  (sub1 x)))
           q))
  '((num bitvec-110)))

;;; possible error
;;;
;;; expected ((num bitvec-111))
;;;
;;; but am getting ((num bitvec-111) (num bitvec-111))
(test "evalo-if-zero?-sub1-b"
  (run* (q)
    (evalo '(let ((x (+ 1 -1)))
              (if (zero? x)
                  x
                  (sub1 x)))
           q))
  '((num bitvec-111)))

(test "evalo-if-zero?-sub1-c"
  (run* (q)
    (evalo '(let ((x 3))
              (if (zero? x)
                  (sub1 x)
                  x))
           q))
  '((num bitvec-100)))


;;; interesting!
;;;
;;; would tabling make this terminate?
;;;
;;; (num bitvec-111) seems unfortunate!
(test "evalo-!-6-simple"
  (run 3 (q)
    (evalo `(letrec ((! (lambda (n)
                          (if (zero? n)
                              1
                              (* n (! (+ n -1)))))))
              (! 6))
           q))
  '((num bitvec-100)
    (num bitvec-111)
    (num bitvec-111)))


;;; would tabling make this terminate?
;;;
;;; sub1 is more specific than - or +
;;;
;;; still getting (num bitvec-111) as an answer
(test "evalo-!-6-simple-classier"
  (run 3 (q)
    (evalo `(letrec ((! (lambda (n)
                          (if (zero? n)
                              1
                              (* n (! (sub1 n)))))))
              (! 6))
           q))
  '((num bitvec-100)
    (num bitvec-110)
    (num bitvec-111)))

#!eof

;;; would tabling make this terminate?
;;;
;;; does this version avoid returning (num bitvec-111)?
;;;
;;; need to support <
(test "evalo-!-6-rich"
  (run 3 (q)
    (evalo `(letrec ((! (lambda (n)
                          (if (< n 0)
                              #f
                              (if (= n 0)
                                  1
                                  (* n (! (sub1 n))))))))
              (! 6))
           q))
  '(720))

#!eof

;;; old and broken tests

;;; FIX this
#|
(test "evalo-backwards-1"
  (run 1 (q)
    (evalo `(+ 0 ',q) '(num bitvec-100)))
  '(3))
|#

(test "evalo-bop-1"
  (run* (q)
    (evalo `((lambda (n) (< n 0)) 0) q))
  '(#f))

(test "evalo-2"
  (run* (q)
    (evalo `(((lambda (f)
                (lambda (n) (if (< n 0) #f
                           (if (= n 0) 1
                               (* n (f (- n 1)))))))
              (lambda (x) 1))
             2)
           q))
  '(2))


(test "evalo-fac-6"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac 6))
           q))
  '(720))

;; slowish
(test "evalo-fac-9"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac 9))
           q))
  '(362880))

(test "evalo-backwards-fac-6"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ,q))
           720))
  '(6))

;; remember the quote!
(test "evalo-backwards-fac-quoted-6"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ',q))
           720))
  '(6))


;; slowish
(test "evalo-backwards-fac-9"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ,q))
           362880))
  '(9))

;; remember the quote!
(test "evalo-backwards-fac-quoted-9"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ',q))
           362880))
  '(9))


;; slowish
(test "evalo-fac-table"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           q))
  '((1 1 2 6)))

(test "evalo-fac-synthesis-hole-0"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) ',q
                                (* n (fac (- n 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           '(1 1 2 6)))
  '(1))

(test "evalo-fac-synthesis-hole-1"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (,q (- n 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           '(1 1 2 6)))
  '(fac))

;; takes a while
(test "evalo-fac-synthesis-hole-1-reversed-examples"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (,q (- n 1))))))))
              (list
               (fac 3)
               (fac 2)
               (fac 1)
               (fac 0)))
           '(6 2 1 1)))
  '(fac))

(test "evalo-fac-synthesis-hole-2"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- ,q 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           '(1 1 2 6)))
  '(n))

(test "evalo-fac-synthesis-hole-3"
  (run 1 (q)
    (fresh (r s)
      (== (list r s) q)
      (evalo `(letrec ((fac
                        (lambda (n)
                          (if (< n 0) #f
                              (if (= n 0) 1
                                  (* n (fac (- ,r ,s))))))))
                (list
                 (fac 0)
                 (fac 1)
                 (fac 2)
                 (fac 3)))
             '(1 1 2 6))))
  '((n 1)))

;; slow, even with the 'symbolo' constraint on 'q'
(test "evalo-fac-synthesis-hole-4"
  (run 1 (q)
    (symbolo q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (,q n 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           '(1 1 2 6)))
  '(-))


(test "evalo-division-using-multiplication-0"
  (run* (q)
    (evalo `(* 3 ',q) 6))
  '(2))

(test "evalo-division-using-multiplication-1"
  (run* (q)
    (evalo `(* 4 ',q) 6))
  '())

(test "evalo-division-using-multiplication-2"
  (run* (q)
    (evalo `(* 3 ',q) 18))
  '(6))

(test "evalo-many-0"
  (run* (q)
    (fresh (x y)
      (evalo `(* ',x ',y) 6)
      (== q (list x y))))
  '((6 1) (1 6) (-1 -6) (-2 -3)
    (-3 -2) (-6 -1) (2 3) (3 2)))

(test "many-1"
  (run* (q)
    (fresh (x y)
      (evalo `(+ (* ',x ',y) (* ',x ',y)) 6)
      (== q (list x y))))
  '((3 1) (1 3) (-1 -3) (-3 -1)))

(test "many-2"
  (run* (q)
    (fresh (x y)
      (evalo `(* (* ',x ',y) 2) 6)
      (== q (list x y))))
  '((3 1) (1 3) (-1 -3) (-3 -1)))

;;; time to get interesting!
