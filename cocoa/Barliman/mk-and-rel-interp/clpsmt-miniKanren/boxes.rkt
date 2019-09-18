#lang racket

(require "mk.rkt")
(do-defer-smt-checks!)
(require graphics/graphics)

(provide (all-from-out "mk.rkt")
         (all-defined-out))

(define (is-boxo b x y w h)
  (fresh ()
    (z/assert `(<= 0 ,x))
    (z/assert `(<= 0 ,y))
    (z/assert `(<= 0 ,w))
    (z/assert `(<= 0 ,h))
    (== b `(box ,x ,y ,w ,h))))

(define (make-boxo w h b)
  (fresh (x y)
    (is-boxo b x y w h)))

(define (withino ax aw bx bw)
  (fresh ()
    (z/assert `(>= ,ax ,bx))
    (z/assert `(<= (+ ,ax ,aw) (+ ,bx ,bw)))))

(define (not-withino ax aw bx bw)
  (fresh ()
    (conde
      ((z/assert `(< ,ax ,bx)))
      ((z/assert `(> (+ ,ax ,aw) (+ ,bx ,bw)))))))

(define (contains-boxo a b)
  (fresh (ax ay aw ah bx by bw bh)
    (is-boxo a ax ay aw ah)
    (is-boxo b bx by bw bh)
    (withino ax aw bx bw)
    (withino ay ah by bh)))

(define (overlaps-boxo a b)
  (fresh (ax ay aw ah bx by bw bh)
    (is-boxo a ax ay aw ah)
    (is-boxo b bx by bw bh)
    (z/assert `(not (= ,ax ,bx)))
    (z/assert `(not (= ,ay ,by)))
    (conde
      ((withino ax aw bx bw)
       (not-withino ay ah by bh))
      ((withino ay ah by bh)
       (not-withino ax aw bx bw)))))

(define (separated-boxo a b)
  (fresh (ax ay aw ah bx by bw bh)
    (is-boxo a ax ay aw ah)
    (is-boxo b bx by bw bh)
    (not-withino ax aw bx bw)
    (not-withino ay ah by bh)))

(define (ex1)
  (define r
    (run 1 (q)
    (fresh (a b c)
      (== q (list a b c))
      (make-boxo 2 2 a)
      (make-boxo 3 3 b)
      (make-boxo 4 4 c)
      (contains-boxo a b)
      (overlaps-boxo b c))))
  (open-graphics)
  (define v (open-viewport "practice" 200 200))
  (define (s x) (* x 20))
  (define (draw-box b)
    ((draw-rectangle v) (make-posn (s (cadr b)) (s (caddr b))) (s (cadddr b)) (s (cadddr (cdr b)))))
  (map draw-box (car r)))
